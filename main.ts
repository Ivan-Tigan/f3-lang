import SWIPL from "npm:swipl-wasm";
async function readStdin(): Promise<string> {
  const decoder = new TextDecoder();
  let input = "";
  for await (const chunk of Deno.stdin.readable) {
    input += decoder.decode(chunk);
  }
  return input;
}
async function main() {
  const p = await readStdin()
    const swipl = await SWIPL({ arguments: ["-q"] });
    await swipl.prolog.load_string(p, "all.pl")
    console.time("prolog_time");
    const res = swipl.prolog.query("findall(Z, p(res,is,Z), AllResults)").once();
    console.timeEnd("prolog_time");
  console.log(res)
}

main();