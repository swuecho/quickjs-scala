// Formatting helpers used by the report modules.

export function currency(value) {
  return "$" + value.toLocaleString("en-US");
}

export function compact(value) {
  if (value >= 1_000_000) return (value / 1_000_000).toFixed(1) + "M";
  if (value >= 1_000) return (value / 1_000).toFixed(1) + "k";
  return String(value);
}

export function percent(value) {
  return (value * 100).toFixed(1) + "%";
}

/** Render a simple left-aligned text table. */
export function table(headers, rows) {
  const widths = headers.map((header, column) =>
    Math.max(header.length, ...rows.map((row) => String(row[column]).length))
  );
  const line = (cells) =>
    cells.map((cell, column) => String(cell).padEnd(widths[column])).join("  ");
  return [line(headers), widths.map((w) => "-".repeat(w)).join("  "), ...rows.map(line)].join("\n");
}
