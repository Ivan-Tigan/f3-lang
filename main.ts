import SWIPL from "npm:swipl-wasm";
import {load, Prolog} from "npm:trealla";
await load();
const pl = new Prolog();

async function readStdin(): Promise<string> {
  const decoder = new TextDecoder();
  let input = "";
  for await (const chunk of Deno.stdin.readable) {
    input += decoder.decode(chunk);
  }
  return input;
}

async function main() {
  const p = await readStdin();
//   pl.fs.open("/fib.pl", {write:true, create:true}).writeString(`
//       :- module(greeting, [fib/2]).
// % fib.pl
// % Base cases
// fib(0, 0).
// fib(1, 1).

// % Recursive rule
// fib(N, F) :-
//     N > 1,
//     N1 is N - 1,
//     N2 is N - 2,
//     fib(N1, F1),
//     fib(N2, F2),
//     F is F1 + F2.

// % Store result in a fact
//     `);
//   pl.consult(`/fib.pl`);
//   console.time("trealla time");
//   const query = pl.query('use_module(greeting), fib(209, Y)');
//   for await (const answer of query) {
//     console.log(answer);
//   }
//   console.timeEnd("trealla time");

  const swipl = await SWIPL({ arguments: ["-q"] });
  await swipl.prolog.load_string(p, "fib.pl");
  console.time("prolog_time");
  const res1 = swipl.prolog.query("result(X)").once();
  console.timeEnd("prolog_time");
  console.log(res1.X);
  //wait 1 sec
  await new Promise(resolve => setTimeout(resolve, 1000));
  console.time("prolog_time_2");
  const res2 = swipl.prolog.query("result(X)").once();
  console.timeEnd("prolog_time_2");
  console.log(res2.X);
}

main();