// @ts-types="npm:@types/parsimmon@^1.10.9"
import P from "npm:parsimmon";
import { parseArgs } from "https://deno.land/std/cli/parse_args.ts";
const pComment = P.string("//").then(P.regexp(/[^\n]*/));
const ws = P.alt(P.whitespace, pComment).many();
const ws1 = P.alt(P.whitespace, pComment).atLeast(1);

const comma = P.string(",").trim(ws);
const semicolon = P.string(";").trim(ws);

// const pNumber = P.regexp(/[0-9]+/).map(r => parseInt(r)).desc("number");
const pNumber = P.regexp(/[-]?[0-9]+(?:\/[0-9]+|\.[0-9]+)?/).desc("number");
const pIdentifier = P.regexp(/[a-zA-Z_][a-zA-Z0-9_-]*/).desc("identifier");
// const pIdentifier = P.regexp(/[a-zA-Z:_][a-zA-Z0-9:_-]*/).desc("identifier");
const pVar = P.regexp(/[?][a-zA-Z0-9_]*/).desc("variable");
// const pSymbol = P.regexp(/[+-=*/^<>~!?|&]+/).desc("symbol");
const pSymbol = P.regexp(/([+\-=*/^<>~!&]|(?!\[|)(?!\|])\|)+/).desc("symbol");
const pString = ws
  .then(
    P.string('"')
      .then(
        P.alt(
          P.string('\\"').result('\\"'),
          P.string("\\\\").result("\\"),
          P.noneOf('"\\')
        )
          .many()
          .map((chars) => chars.join(""))
      )
      .skip(P.string('"'))
      .map((s) => `"${s}"`)
  )
  .desc("string");

type Triple = { s: Node; p: Node; o: Node };

type Node =
  | string
  | number
  | Node[]
  | { type: "chain"; nodes: Node[] }
  | { type: "graph"; triples: Node[] }
  | Triple;
const nestArray = <T>(arr: T[]): [T, T] | [any, T] => {
  if (arr.length === 2) return [arr[0], arr[1]];
  const allButLast = arr.slice(0, -1);
  const last = arr[arr.length - 1];
  return [nestArray(allButLast), last];
};

const pLinkedList: P.Parser<Node[]> = P.string("[|")
  .skip(ws)
  .then(
    P.sepBy(
      P.lazy(() => pNode),
      ws1
    )
  )
  .skip(ws.then(P.string("|]")))
  .map(nestArray);
const pNormalList: P.Parser<Node[]> = P.string("[")
  .skip(ws)
  .then(
    P.sepBy(
      P.lazy(() => pNode),
      ws1
    )
  )
  .skip(ws.then(P.string("]")));
const pList: P.Parser<Node[]> = P.alt(pLinkedList, pNormalList);
const pChain: P.Parser<{ type: "chain"; nodes: Node[] }> = P.sepBy1(
  P.lazy(() => pNodeNoChain),
  P.string(":")
)
  .chain((xs) =>
    xs.length > 1 ? P.succeed(xs) : P.fail("Expected chain with length > 1")
  )
  .map((nodes) => ({ type: "chain", nodes }));

const pGraph = P.string("(")
  .skip(ws)
  .then(P.lazy(() => pTriples().or(P.succeed([]))))
  .skip(ws.then(P.string(")")))
  .map((triples) => ({ type: "graph", triples }));

const pNodeNoChain: P.Parser<Node> = P.alt(
  pString,
  pNumber,
  pIdentifier,
  pVar,
  pList,
  pGraph,
  pSymbol
);
const pNode: P.Parser<Node> = P.alt(pChain, pNodeNoChain);

const pO = (s?: Node): P.Parser<{ o: Node }[] | { tripleTree: Triple }[]> =>
  P.alt(
    P.string("{|")
      .skip(ws)
      .then(P.lazy(() => pTriples(s)))
      .skip(ws.then(P.string("|}")))
      .map((ts) => ts.flat().map((t) => ({ tripleTree: t }))),
    P.string("{")
      .skip(ws)
      .then(P.lazy(() => pTriples()))
      .skip(ws.then(P.string("}")))
      .map((ts) => ts.flat().map((t) => ({ o: t }))),
    pNode.map((o) => [{ o: o }])
  );

const uniques = <T>(arr: T[]) =>
  [...new Set(arr.map((x) => JSON.stringify(x)))].map(
    (x) => JSON.parse(x) as T
  );
const pOs = (s?: Node) => P.sepBy1(pO(s), comma).map((os) => os.flat());
const pPO = (s?: Node) =>
  P.seqMap(pNode.skip(ws1), pOs(s), (p, os) => os.map((o) => ({ p, o })));
const pPOs = (s?: Node) => P.sepBy1(pPO(s), semicolon).map((ps) => ps.flat());
const pSPO = (sPath?: Node) =>
  P.lazy(() => pNode)
    .skip(ws1)
    .chain((s) => {
      const x = pPOs(sPath ? [sPath, s] : s)
        .skip(P.string("."))
        .map((pos) =>
          pos.map(({ p, o }) => ({ s: sPath ? [sPath, s] : s, p, o }))
        );
      return x;
    });
const pTriples = (s?: Node) =>
  P.sepBy1(pSPO(s), ws)
    .map((ts) => ts.flat())
    .trim(ws)
    .map((ts) =>
      ts.flatMap((t) =>
        "tripleTree" in t.o
          ? [
              //@ts-ignore
              { s: t.o.tripleTree.s[0], p: t.p, o: t.o.tripleTree.s },
              t.o.tripleTree,
            ]
          : [{ s: t.s, p: t.p, o: t.o.o }]
      )
    )
    .map(uniques);

const nodeToProlog = (node: Node): string => {
  return typeof node === "string"
    ? node.charAt(0) === "?"
      ? `F3_VAR_PREFIX_${node.slice(1)}`
      : node.charAt(0) === node.charAt(0).toUpperCase() &&
        /[A-Z]/.test(node.charAt(0))
      ? `'${node}'`
      : `${node}`
    : typeof node === "number"
    ? `${node}`
    : Array.isArray(node)
    ? `[${node.map(nodeToProlog).join(", ")}]`
    : "type" in node && node.type === "chain"
    ? (node.nodes.reduceRight(
        (acc, curr) => `chain(${nodeToProlog(curr)}, ${nodeToProlog(acc)})`
      ) as string)
    : "type" in node && node.type === "graph"
    ? `graph([${node.triples.map(nodeToProlog).join(", ")}])`
    : "s" in node && "p" in node && "o" in node
    ? tripleToProlog(node)
    : "";
};

const tripleToProlog = (t: Triple) =>
  `p(${nodeToProlog(t.s)}, ${nodeToProlog(t.p)}, ${nodeToProlog(t.o)})`;
const programToProlog = (ts: Triple[]) =>
  `${ts.map(tripleToProlog).join(".\n")}.`;

async function readStdin(): Promise<string> {
  const decoder = new TextDecoder();
  let input = "";
  for await (const chunk of Deno.stdin.readable) {
    input += decoder.decode(chunk);
  }
  return input;
}

const loadFile = async (filePath: string): Promise<string> => {
  try {
    const content = await Deno.readTextFile(filePath.toString());
    return content;
  } catch (error) {
    console.error(`Error reading file ${filePath}: ${error}`);
    Deno.exit(1);
  }
};

const loadStdin = async (): Promise<string> => {
  if (!Deno.stdin.isTerminal()) {
    const content = await readStdin();
    return content.trim()
      ? content.endsWith("\n")
        ? content
        : content + "\n"
      : "";
  }
  return "";
};

const main = async () => {
  const args = parseArgs(Deno.args);

  const [fileContents, stdinContent] = await Promise.all([
    Promise.all(args._.map((x) => typeof x === "string" && loadFile(x))),
    loadStdin(),
  ]);

  const allInput = fileContents.join("") + stdinContent;

  if (!allInput.trim()) {
    console.error("No input provided");
    Deno.exit(1);
  }
  const result = pTriples().parse(allInput);
  if (result.status) {
    const prolog = programToProlog(result.value);
    console.log(prolog);
  } else {
    // Your error reporting code here
    const lines = allInput.split("\n");
    const errorLine = result.index.line;
    const contextLines = lines.slice(
      Math.max(0, errorLine - 2),
      Math.min(lines.length, errorLine + 2)
    );

    const pointer = " ".repeat(result.index.column) + "^";

    const errorMessage = [
      "\nParse error at line " +
        result.index.line +
        ", column " +
        result.index.column,
      "Expected: " + result.expected.join(", "),
      "\nContext:",
      ...contextLines.map((line, i) => {
        const lineNum = errorLine - 2 + i + 1;
        const prefix = lineNum === errorLine ? ">" : " ";
        return (
          `${prefix} ${lineNum.toString().padStart(3)} | ${line}` +
          (lineNum === errorLine ? "\n    | " + pointer : "")
        );
      }),
      "",
    ].join("\n");

    console.error(errorMessage);
    Deno.exit(1);
  }
};
main();
