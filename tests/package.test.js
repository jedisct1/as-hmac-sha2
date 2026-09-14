import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { copyFileSync, mkdtempSync, readdirSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";
import { fileURLToPath, pathToFileURL } from "node:url";
import loader from "@assemblyscript/loader";

test("the packed Node and AssemblyScript entry points work with production dependencies", async () => {
  const directory = mkdtempSync(join(tmpdir(), "as-hmac-sha2-package-"));
  try {
    execFileSync("npm", ["pack", "--ignore-scripts=false", "--pack-destination", directory], {
      cwd: fileURLToPath(new URL("../", import.meta.url)),
      stdio: "pipe",
    });
    const tarball = readdirSync(directory).find(name => name.endsWith(".tgz"));
    assert.ok(tarball);
    execFileSync("npm", ["install", "--prefix", directory, "--omit=dev", "--ignore-scripts", "--no-audit", "--no-fund", join(directory, tarball)], {
      stdio: "pipe",
    });
    const { default: wasm } = await import(pathToFileURL(join(directory, "node_modules/as-hmac-sha2/index.js")));
    const hex = wasm.__pin(wasm.__newString("00aFff"));
    const bytes = wasm.__pin(wasm.hex2bin(hex));
    try {
      assert.deepEqual(wasm.__getUint8Array(bytes), new Uint8Array([0, 175, 255]));
      assert.equal(wasm.__getString(wasm.bin2hex(bytes)), "00afff");
      assert.equal(wasm.verify(bytes, bytes), 1);
      assert.equal(wasm.hex2bin(wasm.__newString("\u01000")), 0);
    } finally {
      wasm.__unpin(bytes);
      wasm.__unpin(hex);
    }

    copyFileSync(new URL("./consumer.ts", import.meta.url), join(directory, "consumer.ts"));
    execFileSync(process.execPath, [
      fileURLToPath(new URL("../node_modules/assemblyscript/bin/asc.js", import.meta.url)),
      "consumer.ts", "--outFile", "consumer.wasm", "--exportRuntime",
    ], { cwd: directory, stdio: "pipe" });
    const { exports: consumer } = loader.instantiateSync(readFileSync(join(directory, "consumer.wasm")));
    for (const algorithm of ["sha256", "sha512"]) {
      assert.equal(consumer.__getString(consumer[algorithm]()), createHash(algorithm).update("test").digest("hex"));
    }
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
});
