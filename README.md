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

`hex2bin` accepts uppercase and lowercase ASCII hex, and returns `null` for invalid input.

Development requires Node.js 20 or later and npm 10 or later:

```sh
npm install
npm test
npm run test:release
npm run test:long
npm run test:package
```

The long-message test checks SHA-256 and SHA-512 against Node's crypto implementation
at 256 MiB, 512 MiB, and 512 MiB + 1 byte using streamed input. The package test builds
and installs a local tarball with production dependencies, then exercises its Node and
AssemblyScript entry points.
