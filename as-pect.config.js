import { createHash, createHmac } from "node:crypto";

export default {
  /**
   * A set of globs passed to the glob package that qualify typescript files for testing.
   */
  entries: ["assembly/__tests__/**/*.spec.ts"],
  /**
   * A set of globs passed to the glob package that quality files to be added to each test.
   */
  include: ["assembly/__tests__/**/*.include.ts"],
  /**
   * A set of regexp that will disclude source files from testing.
   */
  disclude: [/node_modules/],
  /**
   * Add your required AssemblyScript imports here.
   */
  async instantiate(memory, createImports, instantiate, binary) {
    let instance; // Imports can reference this
    const myImports = {
      env: { memory },
      reference: {
        hash(bits, message) {
          return instance.exports.__newString(
            createHash(`sha${bits}`)
              .update(instance.exports.__getUint8Array(message))
              .digest("hex")
          );
        },
        hmac(bits, message, key) {
          return instance.exports.__newString(
            createHmac(`sha${bits}`, instance.exports.__getUint8Array(key))
              .update(instance.exports.__getUint8Array(message))
              .digest("hex")
          );
        },
        zeroes(bits, length) {
          const hash = createHash(`sha${bits}`);
          const block = Buffer.alloc(1024 * 1024);
          for (let remaining = length; remaining > 0; remaining -= block.length) {
            hash.update(block.subarray(0, Math.min(remaining, block.length)));
          }
          return instance.exports.__newString(hash.digest("hex"));
        },
      }
      // put your web assembly imports here, and return the module promise
    };
    instance = await instantiate(binary, createImports(myImports));
    return instance;
  },
  /** Enable code coverage by uncommenting the following line. */
  // coverage: ["assembly/**/*.ts"],
  /**
   * Specify if the binary wasm file should be written to the file system.
   */
  outputBinary: false,
};
