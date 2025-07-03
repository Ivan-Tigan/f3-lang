:- module(crypto_builtin, []).

:- use_module(library(crypto)).
:- use_module(library(plunit)).

:- multifile user:b/3.
:- multifile user:builtin/1.

% Set test options to show output
:- set_test_options([silent(false)]).

user:builtin([encrypt, _]).
user:builtin([decrypt, _]).

% Symmetric encryption using ChaCha20-Poly1305
user:b(PlainText, [encrypt, Secret], CipherText) :-
    % Use ChaCha20-Poly1305 authenticated encryption
    Algorithm = 'chacha20-poly1305',
    % Derive key from secret (32 bytes for ChaCha20)
    crypto_data_hash(Secret, KeyHash, [algorithm(sha256), encoding(utf8)]),
    hex_bytes(KeyHash, Key),
    % Generate random IV/nonce (12 bytes for ChaCha20-Poly1305)
    crypto_n_random_bytes(12, IV),
    % Encrypt the plaintext (specify octet encoding for binary output)
    crypto_data_encrypt(PlainText, Algorithm, Key, IV, Encrypted, [encoding(utf8), tag(Tag)]),
    % Convert encrypted string to bytes
    string_codes(Encrypted, EncryptedBytes),
    % Combine IV + Tag + Encrypted data for storage
    append(IV, Tag, IVTag),
    append(IVTag, EncryptedBytes, Combined),
    % Convert to hex for easy handling
    hex_bytes(CipherHex, Combined),
    CipherText = CipherHex.

% Symmetric decryption using ChaCha20-Poly1305  
user:b(CipherText, [decrypt, Secret], PlainText) :-
    % Use ChaCha20-Poly1305 authenticated encryption
    Algorithm = 'chacha20-poly1305',
    format(user_error, 'YYYYYYYYYYYYYY decrypt: ~w ~n', [Secret]),
    % Derive key from secret (same as encryption)
    crypto_data_hash(Secret, KeyHash, [algorithm(sha256), encoding(utf8)]),
    hex_bytes(KeyHash, Key),
    % Convert hex back to bytes
    hex_bytes(CipherText, Combined),
    % Extract IV (first 12 bytes), Tag (next 16 bytes), and encrypted data
    append(IV, Rest, Combined), length(IV, 12),
    append(Tag, EncryptedBytes, Rest), length(Tag, 16),
    % Convert encrypted bytes back to string
    string_codes(EncryptedString, EncryptedBytes),
    % Decrypt the data
    crypto_data_decrypt(EncryptedString, Algorithm, Key, IV, PlainText, [encoding(utf8), tag(Tag)]).

% Tests
:- begin_tests(crypto_builtin).

test(encrypt_decrypt_simple) :-
    PlainText = "Hello, World!",
    Secret = "my_secret_key",
    b(PlainText, [encrypt, Secret], CipherText),
    b(CipherText, [decrypt, Secret], DecryptedText),
    PlainText = DecryptedText.

test(encrypt_decrypt_empty) :-
    PlainText = "",
    Secret = "another_secret",
    b(PlainText, [encrypt, Secret], CipherText),
    b(CipherText, [decrypt, Secret], DecryptedText),
    PlainText = DecryptedText.

test(encrypt_decrypt_long_text) :-
    PlainText = "This is a much longer piece of text that should still encrypt and decrypt correctly using the ChaCha20-Poly1305 authenticated encryption algorithm.",
    Secret = "very_secure_secret_key_123",
    b(PlainText, [encrypt, Secret], CipherText),
    b(CipherText, [decrypt, Secret], DecryptedText),
    PlainText = DecryptedText.

test(different_secrets_fail) :-
    PlainText = "Secret message",
    Secret1 = "secret1",
    Secret2 = "secret2", 
    b(PlainText, [encrypt, Secret1], CipherText),
    % Should fail with wrong secret (catch SSL error)
    \+ catch(b(CipherText, [decrypt, Secret2], _), _, fail).

test(encryption_is_randomized) :-
    PlainText = "Same message",
    Secret = "same_secret",
    b(PlainText, [encrypt, Secret], CipherText1),
    b(PlainText, [encrypt, Secret], CipherText2),
    % Should produce different ciphertexts due to random IV
    CipherText1 \= CipherText2.

test(ciphertext_is_hex) :-
    PlainText = "Test message",
    Secret = "test_secret",
    b(PlainText, [encrypt, Secret], CipherText),
    % Should be valid hex string
    hex_bytes(CipherText, _Bytes).

:- end_tests(crypto_builtin).
:- set_test_options([silent(false)]).

:- set_prolog_flag(plunit_output, always).

% Run tests and halt
% :- (run_tests -> halt(0) ; halt(1)).