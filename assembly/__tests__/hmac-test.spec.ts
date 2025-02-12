import { Sha256 } from "../sha256";
import { Sha512 } from "../sha512";
import { bin2hex } from "../utils";
import { decode } from "./base64";

function decodeBase64(input: string): Uint8Array {
  let base64 = "";

  // Manual replacement (instead of regex)
  for (let i = 0; i < input.length; i++) {
    let char = input.charAt(i);
    if (char == "-") {
      base64 += "+";
    } else if (char == "_") {
      base64 += "/";
    } else {
      base64 += char;
    }
  }

  // Add padding if needed
  let padding = (4 - (base64.length % 4)) % 4;
  for (let i = 0; i < padding; i++) {
    base64 += "=";
  }

  return decode(base64);
}

describe("hashing (SHA-512)", (): void => {
  /* jwt.io
   *  HMACSHA512(
   *    base64UrlEncode(header) + "." + base64UrlEncode(payload),
   *    long-enough-nice-and-safe-secret
   *  )
   *  header: {"alg": "HS512", "typ": "JWT"}
   *  payload: {"sub": "1234567890", "name": "John Doe", "iat": 1516239022}
   *
   *  eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.u7oi7__mgFRHXBlbCg-WiW6SxC9CVPJaJRgIcrcpfWGRJw__i7l22ktFE7RoXTJA28WAh4jA5h8H2tS-TChThg
   * */

  it("should validate HMAC (SHA-512)", (): void => {
    let msg = Uint8Array.wrap(
      String.UTF8.encode(
        "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ"
      )
    );
    let key = Uint8Array.wrap(
      String.UTF8.encode("long-enough-nice-and-safe-secret")
    );
    let calculatedSignature = Sha512.hmac(msg, key);
    let calculatedSignature_hex = bin2hex(calculatedSignature);

    let jwtSignature = decodeBase64(
      "u7oi7__mgFRHXBlbCg-WiW6SxC9CVPJaJRgIcrcpfWGRJw__i7l22ktFE7RoXTJA28WAh4jA5h8H2tS-TChThg"
    );
    let providedSignature_hex = bin2hex(jwtSignature);

    expect<string>(providedSignature_hex).toBe(calculatedSignature_hex);
  });

  /* jwt.io
   *  HMACSHA512(
   *    base64UrlEncode(header) + "." + base64UrlEncode(payload),
   *    long-enough-nice-and-safe-secret
   *  )
   *  header: {"alg": "HS512", "typ": "JWT"}
   *  payload: {"sub": "1234567890", "name": "John Doe", "iat": 1516239022, "exp": 1736617739}
   *
   *  eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyLCJleHAiOjE3MzY2MTc3Mzl9.dpDLsxceVlQcF2YLJdw1By3UmYd1pM88OJ8gsfj_qb1NnPnc2TgTknNndO77OCTQEN9XOwyQLplo-LuQwgBSdQ
   * */
  it("should validate HMAC (SHA-512) with extra payload field [exp]", (): void => {
    let msg = Uint8Array.wrap(
      String.UTF8.encode(
        "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyLCJleHAiOjE3MzY2MTc3Mzl9"
      )
    );
    let key = Uint8Array.wrap(
      String.UTF8.encode("long-enough-nice-and-safe-secret")
    );
    let calculatedSignature = Sha512.hmac(msg, key);
    let calculatedSignature_hex = bin2hex(calculatedSignature);

    let jwtSignature = decodeBase64(
      "dpDLsxceVlQcF2YLJdw1By3UmYd1pM88OJ8gsfj_qb1NnPnc2TgTknNndO77OCTQEN9XOwyQLplo-LuQwgBSdQ"
    );
    let providedSignature_hex = bin2hex(jwtSignature);

    expect<string>(providedSignature_hex).toBe(calculatedSignature_hex);
  });

  /* jwt.io
   *  HMACSHA512(
   *    base64UrlEncode(header) + "." + base64UrlEncode(payload),
   *    long-enough-nice-and-safe-secret
   *  )
   *  header: {"alg": "HS512", "typ": "JWT"}
   *  payload: {"sub": "1234567890", "name": "John Doe", "iat": 1516239022, "admin": true}
   *
   *  eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyLCJhZG1pbiI6dHJ1ZX0.9srD9pHI3jrWVcYCvcFrcBZGx3NMI_l8K0SgOe6Rr4yPG3ULz5VfCwEhHSb9skAoe83h7Vbu-M42jCb9Agn1yQ
   * */
  it("should validate HMAC (SHA-512) with extra payload field [admin]", (): void => {
    let msg = Uint8Array.wrap(
      String.UTF8.encode(
        "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyLCJhZG1pbiI6dHJ1ZX0"
      )
    );
    let key = Uint8Array.wrap(
      String.UTF8.encode("long-enough-nice-and-safe-secret")
    );
    let calculatedSignature = Sha512.hmac(msg, key);
    let calculatedSignature_hex = bin2hex(calculatedSignature);

    let jwtSignature = decodeBase64(
      "9srD9pHI3jrWVcYCvcFrcBZGx3NMI_l8K0SgOe6Rr4yPG3ULz5VfCwEhHSb9skAoe83h7Vbu-M42jCb9Agn1yQ"
    );
    let providedSignature_hex = bin2hex(jwtSignature);

    expect<string>(providedSignature_hex).toBe(calculatedSignature_hex);
  });
});

describe("hashing (SHA-256)", (): void => {
  /* jwt.io
   *  HMACSHA256(
   *    base64UrlEncode(header) + "." + base64UrlEncode(payload),
   *    long-enough-nice-and-safe-secret
   *  )
   *  header: {"alg": "HS256", "typ": "JWT"}
   *  payload: {"sub": "1234567890", "name": "John Doe", "iat": 1516239022}
   *
   *  eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.qCoRAbfD5w-hMhBr6MkhBpNKjYwswtlW_gBfCeSmj54
   * */

  it("should validate HMAC (SHA-256)", (): void => {
    let msg = Uint8Array.wrap(
      String.UTF8.encode(
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ"
      )
    );
    let key = Uint8Array.wrap(
      String.UTF8.encode("long-enough-nice-and-safe-secret")
    );
    let calculatedSignature = Sha256.hmac(msg, key);
    let calculatedSignature_hex = bin2hex(calculatedSignature);

    let jwtSignature = decodeBase64(
      "qCoRAbfD5w-hMhBr6MkhBpNKjYwswtlW_gBfCeSmj54"
    );
    let providedSignature_hex = bin2hex(jwtSignature);

    expect<string>(providedSignature_hex).toBe(calculatedSignature_hex);
  });

  /* jwt.io
   *  HMACSHA256(
   *    base64UrlEncode(header) + "." + base64UrlEncode(payload),
   *    long-enough-nice-and-safe-secret
   *  )
   *  header: {"alg": "HS256", "typ": "JWT"}
   *  payload: {"sub": "1234567890", "name": "John Doe", "iat": 1516239022, "exp": 1736617739}
   *
   *  eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyLCJleHAiOjE3MzY2MTc3Mzl9.SA0J70RIIM9TUKSG5jYZhyol3_aN1J-87wgnEZXoTVc
   * */
  it("should validate HMAC (SHA-256) with extra payload field [exp]", (): void => {
    let msg = Uint8Array.wrap(
      String.UTF8.encode(
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyLCJleHAiOjE3MzY2MTc3Mzl9"
      )
    );
    let key = Uint8Array.wrap(
      String.UTF8.encode("long-enough-nice-and-safe-secret")
    );
    let calculatedSignature = Sha256.hmac(msg, key);
    let calculatedSignature_hex = bin2hex(calculatedSignature);

    let jwtSignature = decodeBase64(
      "SA0J70RIIM9TUKSG5jYZhyol3_aN1J-87wgnEZXoTVc"
    );
    let providedSignature_hex = bin2hex(jwtSignature);

    expect<string>(providedSignature_hex).toBe(calculatedSignature_hex);
  });

  /* jwt.io
   *  HMACSHA256(
   *    base64UrlEncode(header) + "." + base64UrlEncode(payload),
   *    long-enough-nice-and-safe-secret
   *  )
   *  header: {"alg": "HS256", "typ": "JWT"}
   *  payload: {"sub": "1234567890", "name": "John Doe", "iat": 1516239022, "admin": true}
   *
   *  eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyLCJhZG1pbiI6dHJ1ZX0.d6OcGHrWC4THwwxxiRmoJyMLCqPXCRX2d4LlmaFLwQ4
   * */
  it("should validate HMAC (SHA-256) with extra payload field [admin]", (): void => {
    let msg = Uint8Array.wrap(
      String.UTF8.encode(
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyLCJhZG1pbiI6dHJ1ZX0"
      )
    );
    let key = Uint8Array.wrap(
      String.UTF8.encode("long-enough-nice-and-safe-secret")
    );
    let calculatedSignature = Sha256.hmac(msg, key);
    let calculatedSignature_hex = bin2hex(calculatedSignature);

    let jwtSignature = decodeBase64(
      "d6OcGHrWC4THwwxxiRmoJyMLCqPXCRX2d4LlmaFLwQ4"
    );
    let providedSignature_hex = bin2hex(jwtSignature);

    expect<string>(providedSignature_hex).toBe(calculatedSignature_hex);
  });
});
