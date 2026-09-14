import fs from "fs";
import loader from "@assemblyscript/loader";
const imports = { /* imports go here */ };
const wasmModule = loader.instantiateSync(fs.readFileSync(new URL("./build/optimized.wasm", import.meta.url)), imports);
export default wasmModule.exports;
