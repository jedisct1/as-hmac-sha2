import fs from "fs";
import loader from "@assemblyscript/loader";
const imports = { /* imports go here */ };
const wasmModule = loader.instantiateSync(fs.readFileSync(__dirname + "/build/optimized.wasm"), imports);
export default wasmModule.exports;
