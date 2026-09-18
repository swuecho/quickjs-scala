#!/usr/bin/env node
// ============================================================
// QuickJS-Scala: crypto toolkit
//
// Run:  java -jar quickjs-runner.jar --node examples/crypto_toolkit.js
//
// Hashes, HMACs, PBKDF2, random values, AES-256-GCM
// authenticated encryption and a UUID — all backed by the JVM.
// ============================================================

const crypto = require("node:crypto");

const out = console.log;
const hex = (buf) => Buffer.from(buf).toString("hex");
const b64 = (buf) => Buffer.from(buf).toString("base64");

function section(n, title) {
  out(`\n── ${n}. ${title} ${"─".repeat(Math.max(2, 52 - title.length - n.toString().length))}`);
}

// ------------------------------------------------------------
section(1, "Hashes (FIPS/RFC vectors)");
// ------------------------------------------------------------
const sha256 = crypto.createHash("sha256").update("abc").digest("hex");
const sha1 = crypto.createHash("sha1").update("abc").digest("hex");
const sha512 = crypto.createHash("sha512").update("").digest("hex");
out("sha256('abc') =", sha256);
out("  expected     ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad:", sha256 === "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
out("sha1('abc')   =", sha1, "(expected a9993e36...)", sha1.startsWith("a9993e36"));
out("sha512('')    =", sha512.slice(0, 32) + "…", "length", sha512.length);

// Streaming updates produce the same digest as one update.
const streamed = crypto.createHash("sha256");
for (const part of ["a", "b", "c"]) streamed.update(part);
out("streamed hash equals one-shot:", streamed.digest("hex") === sha256);

// ------------------------------------------------------------
section(2, "HMAC (RFC 4231 test case 1)");
// ------------------------------------------------------------
const key = Buffer.alloc(20, 0x0b);
const hmac = crypto.createHmac("sha256", key).update("Hi There").digest("hex");
const expectedHmac = "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7";
out("hmac-sha256     =", hmac);
out("matches RFC 4231:", hmac === expectedHmac);

// Constant-time comparison avoids leaking where two buffers differ.
const a = crypto.randomBytes(16);
out("timingSafeEqual:", crypto.timingSafeEqual(a, Buffer.from(a)), crypto.timingSafeEqual(a, Buffer.alloc(16, 255)));

// ------------------------------------------------------------
section(3, "Key derivation (PBKDF2 + HKDF)");
// ------------------------------------------------------------
const salt = Buffer.from("quickjs-scala-demo");
const derived = crypto.pbkdf2Sync("correct horse battery staple", salt, 100_000, 32, "sha256");
out("pbkdf2Sync(100k, 32B) =", b64(derived));

// HKDF is the modern two-step extract-and-expand KDF (RFC 5869).
const hkdf = crypto.hkdfSync("sha256", Buffer.from("input key material"), Buffer.from("salt"), Buffer.from("info"), 32);
out("hkdfSync(32B)         =", b64(hkdf));

// ------------------------------------------------------------
section(4, "Random values and UUIDs");
// ------------------------------------------------------------
const random = crypto.randomBytes(16);
out("randomBytes(16) =", hex(random));
out("randomUUID()    =", crypto.randomUUID());
out("randomInt(1,6)  =", crypto.randomInt(1, 7), "(x5:", Array.from({ length: 5 }, () => crypto.randomInt(1, 7)).join(","), ")");

// ------------------------------------------------------------
section(5, "AES-256-GCM authenticated encryption");
// ------------------------------------------------------------
const secret = crypto.createHash("sha256").update("passphrase").digest(); // 32-byte key
const iv = crypto.randomBytes(12);
const plaintext = JSON.stringify({ account: "ada", balance: 1843, note: "keep secret" });

const cipher = crypto.createCipheriv("aes-256-gcm", secret, iv);
cipher.setAAD(Buffer.from("header:v1"));
const ciphertext = Buffer.concat([cipher.update(plaintext, "utf8"), cipher.final()]);
const tag = cipher.getAuthTag();

out("plaintext :", plaintext);
out("ciphertext:", b64(ciphertext), `(${ciphertext.length} bytes)`);
out("auth tag  :", hex(tag));

const decipher = crypto.createDecipheriv("aes-256-gcm", secret, iv);
decipher.setAAD(Buffer.from("header:v1"));
decipher.setAuthTag(tag);
const decrypted = Buffer.concat([decipher.update(ciphertext), decipher.final()]).toString("utf8");
out("decrypted :", decrypted);
out("round trip ok:", decrypted === plaintext);

// Tampering with the ciphertext makes final() throw instead of returning bad data.
const tampered = Buffer.from(ciphertext);
tampered[0] ^= 0x01;
try {
  const bad = crypto.createDecipheriv("aes-256-gcm", secret, iv);
  bad.setAAD(Buffer.from("header:v1"));
  bad.setAuthTag(tag);
  Buffer.concat([bad.update(tampered), bad.final()]);
  out("tamper detection: FAILED (no error)");
} catch (err) {
  out("tamper detection: rejected as expected");
}

// ------------------------------------------------------------
section(6, "Supported algorithms");
// ------------------------------------------------------------
out("hashes:", crypto.getHashes().filter((h) => ["sha1", "sha256", "sha384", "sha512"].includes(h)).join(", "));
out("ciphers:", crypto.getCiphers().filter((c) => /^aes-(128|192|256)-(gcm|cbc)$/.test(c)).sort().join(", "));
