const fs = require("fs");

async function start() {
    const bytes = fs.readFileSync("./factorial.wasm");
    const env = { log: val => console.log(`logged ${val}`), };
    const { instance } = await WebAssembly.instantiate(
        bytes,
        {
            env: {
                log(val) {
                    console.log(`log saw ${val}`);
                }
            }
        }
    );
    const result = instance.exports.factorial(5);
    console.log(`factorial(5) = ${result}`);
}

start();