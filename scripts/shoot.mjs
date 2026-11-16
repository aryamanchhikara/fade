// Headless Chrome driver used to verify the UI and capture README screenshots.
// No npm dependencies: it talks to Chrome over the DevTools protocol with Node's built-in WebSocket.
//
// usage: node scripts/shoot.mjs <url> <steps.json> [--width 1440] [--height 900]
//   steps.json is a list of steps, run in order:
//     {"wait": ms}                        sleep
//     {"eval": "js expression"}           evaluate, print the result
//     {"waitFor": "js expression"}        poll until truthy (10s timeout)
//     {"shot": "path.png"}                full-page screenshot
import { spawn } from "node:child_process";
import { readFileSync, writeFileSync, mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const args = process.argv.slice(2);
const url = args[0];
const steps = JSON.parse(readFileSync(args[1], "utf8"));
const opt = (name, dflt) => {
  const i = args.indexOf(name);
  return i >= 0 ? Number(args[i + 1]) : dflt;
};
const width = opt("--width", 1440);
const height = opt("--height", 900);
const chrome =
  process.env.CHROME || "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const port = 9300 + Math.floor(Math.random() * 500);
const profile = mkdtempSync(join(tmpdir(), "fade-shoot-"));
const proc = spawn(
  chrome,
  [
    "--headless=new",
    `--remote-debugging-port=${port}`,
    `--user-data-dir=${profile}`,
    `--window-size=${width},${height}`,
    "--force-dark-mode",
    "--hide-scrollbars",
    "about:blank",
  ],
  { stdio: "ignore" },
);
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

async function targetWs() {
  for (let i = 0; i < 50; i++) {
    try {
      const list = await (await fetch(`http://127.0.0.1:${port}/json`)).json();
      const page = list.find((t) => t.type === "page");
      if (page) return page.webSocketDebuggerUrl;
    } catch {}
    await sleep(200);
  }
  throw new Error("chrome did not start");
}

const ws = new WebSocket(await targetWs());
await new Promise((r) => ws.addEventListener("open", r, { once: true }));
let nextId = 1;
const pending = new Map();
const consoleLines = [];
ws.addEventListener("message", (ev) => {
  const msg = JSON.parse(ev.data);
  if (msg.id && pending.has(msg.id)) {
    pending.get(msg.id)(msg);
    pending.delete(msg.id);
  } else if (msg.method === "Runtime.exceptionThrown") {
    consoleLines.push("EXCEPTION " + JSON.stringify(msg.params.exceptionDetails.exception?.description ?? msg.params.exceptionDetails.text));
  } else if (msg.method === "Runtime.consoleAPICalled") {
    consoleLines.push(msg.params.type + " " + msg.params.args.map((a) => a.value ?? a.description).join(" "));
  }
});
const send = (method, params = {}) =>
  new Promise((resolve) => {
    const id = nextId++;
    pending.set(id, resolve);
    ws.send(JSON.stringify({ id, method, params }));
  });
const evaluate = async (expression) => {
  const r = await send("Runtime.evaluate", { expression, awaitPromise: true, returnByValue: true });
  if (r.result?.exceptionDetails) return "EXCEPTION: " + r.result.exceptionDetails.text;
  return r.result?.result?.value;
};

await send("Runtime.enable");
await send("Page.enable");
await send("Emulation.setDeviceMetricsOverride", { width, height, deviceScaleFactor: 2, mobile: false });
await send("Emulation.setEmulatedMedia", { features: [{ name: "prefers-color-scheme", value: "dark" }] });
await send("Page.navigate", { url });
await sleep(800);

let failed = false;
for (const step of steps) {
  if (step.wait) await sleep(step.wait);
  if (step.eval) console.log(JSON.stringify(await evaluate(step.eval)));
  if (step.waitFor) {
    let ok = false;
    for (let i = 0; i < 100 && !ok; i++) {
      ok = Boolean(await evaluate(step.waitFor));
      if (!ok) await sleep(100);
    }
    if (!ok) {
      console.log("TIMEOUT waiting for: " + step.waitFor);
      failed = true;
    }
  }
  if (step.shot) {
    const h = await evaluate("Math.ceil(document.documentElement.scrollHeight)");
    await send("Emulation.setDeviceMetricsOverride", { width, height: Math.max(height, h || height), deviceScaleFactor: 2, mobile: false });
    await sleep(300);
    const r = await send("Page.captureScreenshot", { format: "png", captureBeyondViewport: true });
    writeFileSync(step.shot, Buffer.from(r.result.data, "base64"));
    console.log("wrote " + step.shot);
    await send("Emulation.setDeviceMetricsOverride", { width, height, deviceScaleFactor: 2, mobile: false });
  }
}
for (const l of consoleLines) console.log("[console] " + l);
ws.close();
proc.kill();
process.exit(failed ? 1 : 0);
