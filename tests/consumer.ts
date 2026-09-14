import { Sha256, Sha512, hex2bin, bin2hex } from "as-hmac-sha2";

export function sha256(): string {
  return bin2hex(Sha256.hash(hex2bin("74657374")!));
}

export function sha512(): string {
  return bin2hex(Sha512.hash(hex2bin("74657374")!));
}
