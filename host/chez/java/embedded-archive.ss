;; The read-only java.nio.file view of the application resources baked into an
;; image: a FileSystem over the archive (zip-file.ss embedded-archive-entry-names)
;; and the provider nio-file.ss dispatches its paths through. The archive name is
;; a sentinel, never a host path.

(define embedded-file-system-uri "jar:file:jolt-embedded-resources.jar")
(define embedded-open-file-systems (make-hashtable string-hash string=?))
(define-process-local embedded-file-systems-mutex (make-mutex)) ; merge: upstream's new lock, made process-local as ours are (16d668c5)

;; "jar:file:jolt-embedded-resources.jar" with or without a "!/..." tail names it;
;; any other URI is another archive, which no provider here serves.
(define (embedded-file-system-key uri)
  (let ((s (jolt-str-render-one uri)))
    (and (or (string=? s embedded-file-system-uri)
             (embedded-string-prefix? (string-append embedded-file-system-uri "!/") s))
         embedded-file-system-uri)))
(define (embedded-file-system-key! uri)
  (or (embedded-file-system-key uri)
      (zip-throw "java.nio.file.ProviderNotFoundException"
                 (string-append "Provider not installed for " (jolt-str-render-one uri)))))

(define (embedded-file-system-ensure-open fs)
  (unless (vector-ref (jhost-state fs) 0)
    (zip-throw "java.nio.file.ClosedFileSystemException" "embedded resource filesystem is closed")))

;; #(open? file-store provider-object)
(define (embedded-file-system-new uri . env)
  (let ((key (embedded-file-system-key! uri)))
    (jolt-with-mutex embedded-file-systems-mutex
      (when (hashtable-ref embedded-open-file-systems key #f)
        (zip-throw "java.nio.file.FileSystemAlreadyExistsException" key))
      (let ((fs (make-jhost "embedded-nio-filesystem" (vector #t #f #f))))
        (hashtable-set! embedded-open-file-systems key fs)
        fs))))
(define (embedded-file-system-get uri)
  (let ((key (embedded-file-system-key! uri)))
    (or (jolt-with-mutex embedded-file-systems-mutex
          (hashtable-ref embedded-open-file-systems key #f))
        (zip-throw "java.nio.file.FileSystemNotFoundException" key))))
(define (embedded-file-system-close fs)
  (when (vector-ref (jhost-state fs) 0)
    (vector-set! (jhost-state fs) 0 #f)
    (jolt-with-mutex embedded-file-systems-mutex
      (when (eq? fs (hashtable-ref embedded-open-file-systems embedded-file-system-uri #f))
        (hashtable-delete! embedded-open-file-systems embedded-file-system-uri))))
  jolt-nil)

(define (embedded-file-system-path fs first . more)
  (embedded-file-system-ensure-open fs)
  (npath-get-strings-on fs first more))
(define (embedded-file-system-roots fs)
  (embedded-file-system-ensure-open fs)
  (list->cseq (list (make-nio-path-on fs "/"))))
(define (embedded-file-system-matcher fs spec)
  (embedded-file-system-ensure-open fs)
  (npath-make-matcher-on #f (npath-string-of spec)))

;; ---- paths and what they name ---------------------------------------------------
;; A path's key is its normalized absolute spelling; the resource it names drops
;; the leading "/".
(define (embedded-nio-key path)
  (let ((s (npath-normalize-for #f (nio-path-str path))))
    (cond ((or (string=? s "") (string=? s "/")) "/")
          ((char=? (string-ref s 0) #\/) s)
          (else (string-append "/" s)))))
(define (embedded-nio-resource-name path)
  (let ((key (embedded-nio-key path)))
    (if (string=? key "/") "" (substring key 1 (string-length key)))))
(define (embedded-nio-kind path)
  (let ((name (embedded-nio-resource-name path)))
    (cond ((string=? name "") 'directory)
          ((embedded-public-resource? name) 'file)
          ((embedded-directory? name) 'directory)
          (else #f))))
(define (embedded-nio-require path)
  (or (embedded-nio-kind path) (nio-no-such-file (nio-path-str path))))
(define (embedded-nio-bytes path)
  (embedded-file-system-ensure-open (nio-path-filesystem path))
  (if (eq? (embedded-nio-require path) 'directory)
      (nio-fs-detail (nio-path-str path) "Is a directory")
      ;; the base entry: a zip filesystem reads versioned entries only when it
      ;; is opened with a releaseVersion, which this one never is
      (let ((content (embedded-resource-ref (embedded-nio-resource-name path))))
        (if (bytevector? content) content (string->utf8 content)))))

;; Path.toUri: the archive's URI, "!", and the absolute path quoted the way the
;; multi-argument URI constructor quotes it; Path.of(URI) reads it back.
(define (embedded-nio-path-uri path)
  (embedded-file-system-ensure-open (nio-path-filesystem path))
  (uri-ctor (string-append embedded-file-system-uri "!" (embedded-uri-encode (embedded-nio-key path)))))
(define (embedded-uri->path uri)
  (let* ((fs (embedded-file-system-get uri))
         (s (jolt-str-render-one uri))
         (bang (let find ((i 0))
                 (cond ((>= i (string-length s)) #f)
                       ((char=? (string-ref s i) #\!) i)
                       (else (find (+ i 1)))))))
    (make-nio-path-on fs (if bang (embedded-percent-decode (substring s (+ bang 1) (string-length s))) "/"))))

;; The children of a directory, in name order, each spelled under the
;; directory's own spelling (absolute or not).
(define (embedded-nio-children path)
  (unless (eq? (embedded-nio-require path) 'directory)
    (nio-fs-throw "java.nio.file.NotDirectoryException" (nio-path-str path)))
  (let* ((key (embedded-nio-key path))
         (raw (npath-normalize-for #f (nio-path-str path)))
         (prefix (if (string=? key "/") "" (string-append (substring key 1 (string-length key)) "/")))
         (base-path (if (npath-absolute-for? #f raw) key raw))
         (base (cond ((string=? base-path "") "")
                     ((string=? base-path "/") "/")
                     (else (string-append base-path "/"))))
         (seen (make-hashtable string-hash string=?)))
    (let loop ((names (if (string=? prefix "") (embedded-resource-names) (embedded-resources-under prefix)))
               (acc '()))
      (if (null? names)
          (reverse acc)
          (let* ((rest (substring (car names) (string-length prefix) (string-length (car names))))
                 (slash (let find ((i 0))
                          (cond ((>= i (string-length rest)) #f)
                                ((char=? (string-ref rest i) #\/) i)
                                (else (find (+ i 1))))))
                 (child (if slash (substring rest 0 slash) rest)))
            (if (hashtable-ref seen child #f)
                (loop (cdr names) acc)
                (begin
                  (hashtable-set! seen child #t)
                  (loop (cdr names) (cons (npath-like path (string-append base child)) acc)))))))))

;; ---- attributes -------------------------------------------------------------------
(define (embedded-nio-attribute path name)
  (let ((kind (embedded-nio-require path)))
    (cond
      ((member name '("lastModifiedTime" "lastAccessTime" "creationTime")) (make-file-time 0))
      ((string=? name "size") (if (eq? kind 'directory) 0 (bytevector-length (embedded-nio-bytes path))))
      ((string=? name "isDirectory") (eq? kind 'directory))
      ((string=? name "isRegularFile") (eq? kind 'file))
      ((string=? name "isSymbolicLink") #f)
      ((string=? name "isOther") #f)
      (else jolt-nil))))
(define (make-embedded-basic-attrs path)
  (make-jhost "embedded-basic-attrs" path))
(register-host-methods! "embedded-basic-attrs"
  (map (lambda (name)
         (cons name (lambda (self) (embedded-nio-attribute (jhost-state self) name))))
       '("isDirectory" "isRegularFile" "isSymbolicLink" "isOther" "size"
         "lastModifiedTime" "lastAccessTime" "creationTime" "fileKey")))
(hashtable-set! jhost-tag->fqn "embedded-basic-attrs" "java.nio.file.attribute.BasicFileAttributes")

;; Only the "basic" view: any other view or attribute name is refused.
(define (embedded-nio-view! what)
  (let* ((text (npath-string-of what))
         (colon (let find ((i 0))
                  (cond ((>= i (string-length text)) #f)
                        ((char=? (string-ref text i) #\:) i)
                        (else (find (+ i 1)))))))
    (when (and colon (not (string=? (substring text 0 colon) "basic")))
      (throw-jvm 'UnsupportedOperationException
                 (string-append "unsupported file attribute view: " (substring text 0 colon))))))
(define (embedded-nio-attribute! path name)
  (unless (member name nio-basic-attr-names)
    (throw-jvm 'IllegalArgumentException (string-append "unknown basic attribute: " name)))
  (embedded-nio-attribute path name))
(define (embedded-nio-read-attributes path what . opts)
  (embedded-nio-require path)
  (if (jclass? what)
      (if (string=? (jclass-name what) "java.nio.file.attribute.BasicFileAttributes")
          (make-embedded-basic-attrs path)
          (throw-jvm 'UnsupportedOperationException
                     (string-append "unsupported attributes class: " (jclass-name what))))
      (let ((w (npath-string-of what)))
        (embedded-nio-view! w)
        (let* ((part (nio-attr-name w))
               (names (if (string=? part "*") nio-basic-attr-names (nio-split-commas part))))
          (fold-left (lambda (m name) (jolt-assoc m name (embedded-nio-attribute! path name)))
                     empty-pmap names)))))

(define (embedded-nio-directory-stream path . filter-arg)
  (let ((arg (and (pair? filter-arg) (car filter-arg)))
        (paths (embedded-nio-children path)))
    (make-dir-stream
     (cond
       ((string? arg)
        (let ((rx (jolt-re-pattern (npath-glob->regex-for #f arg))))
          (filter (lambda (child)
                    (jolt-truthy?
                     (jolt-re-matches rx (npath-string-of (npath-file-name-on (nio-path-filesystem child)
                                                                            (nio-path-str child))))))
                  paths)))
       ((and arg (reify-method-ref arg "accept"))
        => (lambda (m) (filter (lambda (child) (jolt-truthy? (jolt-invoke m arg child))) paths)))
       (else paths)))))

(define (embedded-nio-walk start . args)
  (let* ((visitor (list-ref args (- (length args) 1)))
         (max-depth (if (= (length args) 3) (cadr args) 2147483647))
         (md (if (number? max-depth) (exact (truncate max-depth)) 2147483647)))
    (call/cc
     (lambda (stop)
       (define (walk path depth)
         (if (and (eq? (embedded-nio-require path) 'directory) (< depth md))
             (let ((result (nio-call-visitor visitor "preVisitDirectory" path
                                             (make-embedded-basic-attrs path))))
               (cond
                 ((eq? result 'terminate) (stop #t))
                 ((eq? result 'skip-subtree) 'continue)
                 ((eq? result 'skip-siblings) 'skip-siblings)
                 (else
                  (let loop ((children (embedded-nio-children path)))
                    (unless (null? children)
                      (let ((child-result (walk (car children) (+ depth 1))))
                        (unless (eq? child-result 'skip-siblings)
                          (loop (cdr children))))))
                  (let ((post (nio-call-visitor visitor "postVisitDirectory" path jolt-nil)))
                    (if (eq? post 'terminate) (stop #t) 'continue)))))
             (let ((result (nio-call-visitor visitor "visitFile" path
                                             (make-embedded-basic-attrs path))))
               (if (eq? result 'terminate) (stop #t) result))))
       (walk start 0)))
    start))

;; ---- channels ---------------------------------------------------------------------
;; A read-only SeekableByteChannel over a resource's bytes. State: #(bytes position open?)
(define (embedded-channel-ensure-open self)
  (unless (vector-ref (jhost-state self) 2)
    (throw-jvm 'java.nio.channels.ClosedChannelException "embedded channel is closed")))
(define (embedded-channel-position self) (vector-ref (jhost-state self) 1))
(define (embedded-channel-position-set! self position)
  (embedded-channel-ensure-open self)
  (let ((position (jnum->exact position)))
    (when (< position 0)
      (throw-jvm 'IllegalArgumentException "negative channel position"))
    (vector-set! (jhost-state self) 1 position)
    self))
(define (embedded-channel-read self buffer)
  (embedded-channel-ensure-open self)
  (let* ((bytes (vector-ref (jhost-state self) 0))
         (position (embedded-channel-position self))
         (n (min (- (bytevector-length bytes) position) (nb-remaining buffer))))
    (cond ((= (nb-remaining buffer) 0) (->num 0))
          ((<= n 0) (->num -1))
          (else
           (let ((slice (make-bytevector n)))
             (bytevector-copy! bytes position slice 0 n)
             (nb-put buffer (na-byte-array slice) 0 n)
             (embedded-channel-position-set! self (+ position n))
             (->num n))))))
(register-host-methods! "embedded-byte-channel"
  (list
   (cons "read" embedded-channel-read)
   (cons "write" (lambda (self buffer)
                   (embedded-channel-ensure-open self)
                   (zip-throw "java.nio.file.ReadOnlyFileSystemException" "write")))
   (cons "position" (lambda (self . position)
                      (embedded-channel-ensure-open self)
                      (if (pair? position)
                          (embedded-channel-position-set! self (car position))
                          (embedded-channel-position self))))
   (cons "size" (lambda (self)
                  (embedded-channel-ensure-open self)
                  (->num (bytevector-length (vector-ref (jhost-state self) 0)))))
   (cons "truncate" (lambda (self size)
                      (embedded-channel-ensure-open self)
                      (zip-throw "java.nio.file.ReadOnlyFileSystemException" "truncate")))
   (cons "isOpen" (lambda (self) (vector-ref (jhost-state self) 2)))
   (cons "close" (lambda (self) (vector-set! (jhost-state self) 2 #f) jolt-nil))))
(hashtable-set! jhost-tag->fqn "embedded-byte-channel" "java.nio.channels.SeekableByteChannel")
(hashtable-set! jhost-tag->fqn "embedded-file-channel" "java.nio.channels.FileChannel")
(alias-host-methods! "embedded-file-channel" "embedded-byte-channel")
(define (embedded-channel tag)
  (lambda (path . options)
    (when (exists (lambda (option)
                    (and (jhost? option)
                         (member (enum-name option)
                                 '("WRITE" "APPEND" "TRUNCATE_EXISTING" "CREATE" "CREATE_NEW" "DELETE_ON_CLOSE"))))
                  (npath-spread-args options))
      (zip-throw "java.nio.file.ReadOnlyFileSystemException" "open for writing"))
    (make-jhost tag (vector (embedded-nio-bytes path) 0 #t))))

;; ---- stores, views, principals and watch services ------------------------------------
(define (embedded-file-system-cache! fs index value)
  (or (vector-ref (jhost-state fs) index)
      (let ((created (value)))
        (vector-set! (jhost-state fs) index created)
        created)))
(define (embedded-file-store fs)
  (embedded-file-system-cache! fs 1 (lambda () (make-jhost "embedded-file-store" fs))))
(define (embedded-file-system-provider fs)
  (embedded-file-system-cache! fs 2 (lambda () (make-jhost "embedded-file-system-provider" fs))))
(register-host-methods! "embedded-file-store"
  (list
   (cons "name" (lambda (self) "jolt-embedded-resources"))
   (cons "type" (lambda (self) "embedded"))
   (cons "isReadOnly" (lambda (self) #t))
   (cons "getTotalSpace" (lambda (self) 0))
   (cons "getUsableSpace" (lambda (self) 0))
   (cons "getUnallocatedSpace" (lambda (self) 0))
   (cons "getBlockSize" (lambda (self) 0))
   (cons "getBlockCount" (lambda (self) 0))
   (cons "getFreeBlocks" (lambda (self) 0))
   (cons "getAvailableBlocks" (lambda (self) 0))
   (cons "supportsFileAttributeView" (lambda (self view) (string=? (nio-view-name view) "basic")))
   (cons "getAttribute" (lambda (self attribute) jolt-nil))
   (cons "getFileStoreAttributeView" (lambda (self view) jolt-nil))))

(define (embedded-principal-lookup self name)
  (throw-jvm 'UnsupportedOperationException "embedded resources have no user principals"))
(register-host-methods! "embedded-principal-lookup"
  (list (cons "lookupPrincipalByName" embedded-principal-lookup)
        (cons "lookupPrincipalByGroupName" embedded-principal-lookup)))

(define (embedded-watch-key path)
  (make-jhost "embedded-watch-key" (vector path #t)))
(register-host-methods! "embedded-watch-key"
  (list
   (cons "watchable" (lambda (self) (vector-ref (jhost-state self) 0)))
   (cons "pollEvents" (lambda (self) (make-pvec (vector))))
   (cons "reset" (lambda (self) (vector-ref (jhost-state self) 1)))
   (cons "cancel" (lambda (self) (vector-set! (jhost-state self) 1 #f) jolt-nil))
   (cons "isValid" (lambda (self) (vector-ref (jhost-state self) 1)))))
(define (embedded-watch-service fs)
  (embedded-file-system-ensure-open fs)
  (make-jhost "embedded-watch-service" (vector fs #t)))
(define (embedded-watch-service-ensure-open self)
  (unless (vector-ref (jhost-state self) 1)
    (zip-throw "java.nio.file.ClosedWatchServiceException" "watch service is closed"))
  (embedded-file-system-ensure-open (vector-ref (jhost-state self) 0)))
(define (embedded-watch-service-poll self)
  (embedded-watch-service-ensure-open self)
  jolt-nil)
(register-host-methods! "embedded-watch-service"
  (list
   (cons "poll" (lambda (self . timeout) (embedded-watch-service-poll self)))
   (cons "take" (lambda (self) (embedded-watch-service-poll self)))
   (cons "close" (lambda (self) (vector-set! (jhost-state self) 1 #f) jolt-nil))))
(define (embedded-nio-path-register path service . events)
  (unless (and (jhost? service) (string=? (jhost-tag service) "embedded-watch-service"))
    (throw-jvm 'IllegalArgumentException "embedded paths require an embedded watch service"))
  (embedded-watch-service-ensure-open service)
  (embedded-nio-require path)
  (embedded-watch-key path))

;; ---- the provider ------------------------------------------------------------------
;; Its Files table holds the primitives; text, path streams, comparison and
;; copies to other filesystems come from nio-file.ss's shared operations over
;; them. Every write is ReadOnlyFileSystemException, and every operation first
;; refuses a closed filesystem.
(define (embedded-read-only operation)
  (lambda args (zip-throw "java.nio.file.ReadOnlyFileSystemException" operation)))
(define (embedded-same-file? a b)
  (and (nio-path? b)
       (eq? (nio-path-filesystem a) (nio-path-filesystem b))
       (or (string=? (embedded-nio-key a) (embedded-nio-key b))
           (begin (embedded-nio-require a) (embedded-nio-require b) #f))))
(define (embedded-copy source destination . opts)
  (cond
    ((nio-path? destination) (zip-throw "java.nio.file.ReadOnlyFileSystemException" "copy"))
    ((not (nio-path? source)) (zip-throw "java.nio.file.ReadOnlyFileSystemException" "copy"))
    (else
     (let ((bytes (embedded-nio-bytes source)))
       (record-method-dispatch destination "write"
         (list->cseq (list (na-byte-array bytes) (->num 0) (->num (bytevector-length bytes)))))
       (->num (bytevector-length bytes))))))
(define embedded-files-ops
  (append
   (map (lambda (op) (cons op (embedded-read-only op)))
        '("delete" "deleteIfExists" "write" "newOutputStream"
          "createDirectory" "createDirectories" "createFile" "createTempFile" "createTempDirectory"
          "createSymbolicLink" "createLink" "move" "setOwner"
          "setLastModifiedTime" "setAttribute" "setPosixFilePermissions"))
   (list
    (cons "exists" (lambda (path . opts) (and (embedded-nio-kind path) #t)))
    (cons "notExists" (lambda (path . opts) (not (embedded-nio-kind path))))
    (cons "isDirectory" (lambda (path . opts) (eq? (embedded-nio-kind path) 'directory)))
    (cons "isRegularFile" (lambda (path . opts) (eq? (embedded-nio-kind path) 'file)))
    (cons "isSymbolicLink" (lambda (path) #f))
    (cons "isReadable" (lambda (path) (and (embedded-nio-kind path) #t)))
    (cons "isWritable" (lambda (path) #f))
    (cons "isExecutable" (lambda (path) #f))
    (cons "isHidden"
          (lambda (path)
            (let ((name (npath-string-of (npath-file-name-on (nio-path-filesystem path) (nio-path-str path)))))
              (and (> (string-length name) 0) (char=? (string-ref name 0) #\.)))))
    (cons "size" (lambda (path) (embedded-nio-attribute path "size")))
    (cons "readAllBytes" (lambda (path) (na-bv->bytearray (embedded-nio-bytes path))))
    (cons "newInputStream" (lambda (path . opts) (make-in-stream (open-bytevector-input-port (embedded-nio-bytes path)))))
    (cons "newByteChannel" (embedded-channel "embedded-byte-channel"))
    (cons "newDirectoryStream" embedded-nio-directory-stream)
    (cons "readAttributes" embedded-nio-read-attributes)
    (cons "getAttribute" (lambda (path what . opts)
                           (let ((w (npath-string-of what)))
                             (embedded-nio-view! w)
                             (embedded-nio-attribute! path (nio-attr-name w)))))
    (cons "getLastModifiedTime" (lambda (path . opts) (embedded-nio-attribute path "lastModifiedTime")))
    (cons "getFileAttributeView"
          (nio-attribute-view '("basic") (lambda (what) (zip-throw "java.nio.file.ReadOnlyFileSystemException" what))))
    (cons "getFileStore" (lambda (path)
                           (embedded-nio-require path)
                           (embedded-file-store (nio-path-filesystem path))))
    (cons "walkFileTree" embedded-nio-walk)
    (cons "isSameFile" embedded-same-file?)
    (cons "copy" embedded-copy))))
(define (embedded-open-checked f)
  (lambda args
    (let ((path (find nio-path? (if (and (pair? args) (pair? (cdr args))) (list (car args) (cadr args)) args))))
      (when (and path (jhost? (nio-path-filesystem path))
                 (string=? (jhost-tag (nio-path-filesystem path)) "embedded-nio-filesystem"))
        (embedded-file-system-ensure-open (nio-path-filesystem path))))
    (apply f args)))
(define embedded-nio-provider
  (make-nio-provider
   "jar"
   (lambda () #f)
   (lambda (s) s)
   (nio-alist->table (map (lambda (p) (cons (car p) (embedded-open-checked (cdr p)))) embedded-files-ops))
   (nio-alist->table
    (list
     (cons "toAbsolutePath" (lambda (path)
                              (embedded-file-system-ensure-open (nio-path-filesystem path))
                              (npath-like path (embedded-nio-key path))))
     (cons "toRealPath" (lambda (path . opts)
                          (embedded-file-system-ensure-open (nio-path-filesystem path))
                          (embedded-nio-require path)
                          (npath-like path (embedded-nio-key path))))
     (cons "toFile" (lambda (path) (throw-jvm 'UnsupportedOperationException jolt-nil)))
     (cons "toUri" embedded-nio-path-uri)
     (cons "register" embedded-nio-path-register)
     (cons "newFileChannel" (embedded-open-checked (embedded-channel "embedded-file-channel")))))
   embedded-uri->path
   embedded-file-system-new
   embedded-file-system-get
   embedded-nio-bytes))
(register-nio-provider! "embedded-nio-filesystem" embedded-nio-provider)

;; FileSystem.provider(): the FileSystemProvider object, whose members are the
;; same operations Files dispatches to.
(define (embedded-provider-check-access self path . modes)
  (embedded-nio-require path)
  (when (exists (lambda (mode) (and (jhost? mode) (member (enum-name mode) '("WRITE" "EXECUTE"))))
                (npath-spread-args modes))
    (nio-fs-throw "java.nio.file.AccessDeniedException" (nio-path-str path)))
  jolt-nil)
(define (embedded-provider-new-file-system self first . rest)
  (cond
    ((uri-jhost? first) (embedded-file-system-new first))
    ((nio-path? first) (nio-path-filesystem first))
    (else (throw-jvm 'UnsupportedOperationException "unsupported filesystem argument"))))
(define (embedded-provider-files op)
  (lambda (self . args) (apply nio-files-call op args)))
(register-host-methods! "embedded-file-system-provider"
  (append
   (list
    (cons "getScheme" (lambda (self) "jar"))
    (cons "newFileSystem" embedded-provider-new-file-system)
    (cons "getFileSystem" (lambda (self uri) (embedded-file-system-get uri)))
    (cons "getPath" (lambda (self uri) (embedded-uri->path uri)))
    (cons "newFileChannel" (lambda (self path . opts) (apply (nio-path-op path "newFileChannel") path opts)))
    (cons "newAsynchronousFileChannel" (lambda (self path . opts)
                                         (throw-jvm 'UnsupportedOperationException jolt-nil)))
    (cons "checkAccess" embedded-provider-check-access)
    (cons "readAttributesIfExists"
          (lambda (self path what . opts)
            (if (embedded-nio-kind path) (apply embedded-nio-read-attributes path what opts) jolt-nil)))
    (cons "exists" (lambda (self path . opts) (and (embedded-nio-kind path) #t))))
   (map (lambda (op) (cons op (embedded-provider-files op)))
        '("newInputStream" "newOutputStream" "newByteChannel" "newDirectoryStream" "createDirectory"
          "createSymbolicLink" "createLink" "delete" "deleteIfExists" "readSymbolicLink" "copy"
          "move" "isSameFile" "isHidden" "getFileStore" "getFileAttributeView" "readAttributes"
          "setAttribute"))))

(register-enum-class! "java.nio.file.AccessMode"
  (list (cons "READ" (make-jhost "access-mode" 'read))
        (cons "WRITE" (make-jhost "access-mode" 'write))
        (cons "EXECUTE" (make-jhost "access-mode" 'execute))))
(hashtable-set! jhost-tag->fqn "embedded-file-store" "java.nio.file.FileStore")
(hashtable-set! jhost-tag->fqn "embedded-file-system-provider" "java.nio.file.spi.FileSystemProvider")
(hashtable-set! jhost-tag->fqn "embedded-principal-lookup" "java.nio.file.attribute.UserPrincipalLookupService")
(hashtable-set! jhost-tag->fqn "embedded-watch-service" "java.nio.file.WatchService")
(hashtable-set! jhost-tag->fqn "embedded-watch-key" "java.nio.file.WatchKey")
(hashtable-set! jhost-tag->fqn "embedded-nio-filesystem" "java.nio.file.FileSystem")
(register-host-methods! "embedded-nio-filesystem"
  (list
   (cons "provider" embedded-file-system-provider)
   (cons "getPath" embedded-file-system-path)
   (cons "getPathMatcher" embedded-file-system-matcher)
   (cons "getSeparator" (lambda (fs) "/"))
   (cons "getRootDirectories" embedded-file-system-roots)
   (cons "getFileStores" (lambda (fs) (list->cseq (list (embedded-file-store fs)))))
   (cons "supportedFileAttributeViews" (lambda (fs) (jolt-hash-set "basic")))
   (cons "getUserPrincipalLookupService" (lambda (fs) (make-jhost "embedded-principal-lookup" fs)))
   (cons "newWatchService" embedded-watch-service)
   (cons "isReadOnly" (lambda (fs) #t))
   (cons "isOpen" (lambda (fs) (vector-ref (jhost-state fs) 0)))
   (cons "close" embedded-file-system-close)))
