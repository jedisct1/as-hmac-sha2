# HMAC-SHA-256 and HMAC-SHA-512 for AssemblyScript

Self-contained implementations of SHA-256, SHA-512, HMAC-SHA-256 and HMAC-SHA-512 for AssemblyScript.

Simple hashing:

```typescript
let msg = Uint8Array.wrap(String.UTF8.encode("test"));
let h = Sha256.hash(msg);
```

Chunked input:

```typescript
let st = new Sha256();
st.update(msg1);
st.update(msg2);
let h = st.final();
```

HMAC:

```typescript
let msg = Uint8Array.wrap(String.UTF8.encode("message"));
let key = Uint8Array.wrap(String.UTF8.encode("key"));
let mac = Sha256.hmac(msg, key);
```

Constant-time check for equality:

```typescript
let ok = verify(mac, expected_mac);
```

Constant-time hexadecimal encoding/decoding:

```typescript
let hex = bin2hex(h);
let bin = hex2bin(hex);
```