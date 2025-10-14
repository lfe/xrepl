(defmodule xrepl-msgpack
  "MessagePack encoding/decoding for xrepl protocol.

  Wraps msgpack-erlang library with xrepl-specific helpers."
  (export
   (encode 1)
   (decode 1)
   (encode-with-length 1)
   (decode-with-length 1)))

(defun encode (term)
  "Encode Erlang term as MessagePack binary.

  Args:
    term: Erlang term (map, list, integer, binary, etc.)

  Returns:
    {ok, binary} | {error, reason}"
  (try
    (tuple 'ok (msgpack:pack term))
    (catch
      ((tuple error-type reason _)
       (tuple 'error (tuple error-type reason))))))

(defun decode (binary)
  "Decode MessagePack binary to Erlang term.

  Args:
    binary: MessagePack-encoded binary

  Returns:
    {ok, term} | {error, reason}"
  (try
    (case (msgpack:unpack binary)
      (`#(ok ,term)
       (tuple 'ok term))
      (`#(error ,reason)
       (tuple 'error reason)))
    (catch
      ((tuple error-type reason _)
       (tuple 'error (tuple error-type reason))))))

(defun encode-with-length (term)
  "Encode term with 4-byte length prefix.

  Wire format: [Length:32/big][MessagePack Data]

  Args:
    term: Erlang term to encode

  Returns:
    {ok, binary} | {error, reason}"
  (case (encode term)
    (`#(ok ,msgpack-data)
     (let* ((length (byte_size msgpack-data))
            (length-prefix (binary:encode_unsigned length 'big))
            ;; Pad to 4 bytes
            (padded-length (pad-to-4-bytes length-prefix)))
       (tuple 'ok (binary:list_to_bin (list padded-length msgpack-data)))))
    (error error)))

(defun decode-with-length (binary)
  "Decode binary with 4-byte length prefix.

  Args:
    binary: Binary with format [Length:32/big][MessagePack Data]

  Returns:
    {ok, term, rest} | {error, reason}
    where rest is any remaining bytes after the message"
  (if (< (byte_size binary) 4)
    (tuple 'error 'incomplete-length)
    (let* ((length-bytes (binary:part binary 0 4))
           (expected-length (binary:decode_unsigned length-bytes 'big))
           (total-expected (+ 4 expected-length)))
      (if (< (byte_size binary) total-expected)
        (tuple 'error 'incomplete-message)
        (let* ((msgpack-data (binary:part binary 4 expected-length))
               (rest (binary:part binary total-expected
                                 (- (byte_size binary) total-expected))))
          (case (decode msgpack-data)
            (`#(ok ,term)
             (tuple 'ok term rest))
            (error error)))))))

;;; Private Functions

(defun pad-to-4-bytes (binary)
  "Pad binary to 4 bytes (big endian)."
  (let ((size (byte_size binary)))
    (case size
      (4 binary)
      (3 (binary:list_to_bin (list #"\0" binary)))
      (2 (binary:list_to_bin (list #"\0\0" binary)))
      (1 (binary:list_to_bin (list #"\0\0\0" binary)))
      (0 #"\0\0\0\0")
      (_ binary))))  ;; Should not happen
