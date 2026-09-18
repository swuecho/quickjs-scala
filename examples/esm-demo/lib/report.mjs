// Lazily imported by main.mjs (dynamic import) when --table is passed.

import { compact, currency, percent } from "../lib/format.mjs";
import { mean } from "../lib/stats.mjs";

export function render({ quarters, regions }) {
  const header = ["Region", ...quarters, "Total", "Avg", "Share"];
  const grandTotal = regions.reduce(
    (total, region) => total + region.sales.reduce((a, b) => a + b, 0),
    0
  );
  const rows = regions.map((region) => {
    const regionTotal = region.sales.reduce((a, b) => a + b, 0);
    return [
      region.name,
      // Spread inside an array literal between other elements.
      ...region.sales.map(currency),
      currency(regionTotal),
      currency(Math.round(mean(region.sales))),
      percent(regionTotal / grandTotal),
    ];
  });
  const totalRow = [
    "All regions",
    ...quarters.map((_, index) =>
      compact(regions.reduce((total, region) => total + region.sales[index], 0))
    ),
    currency(grandTotal),
    currency(Math.round(grandTotal / regions.length / quarters.length)),
    "100.0%",
  ];
  return { header, rows: [...rows, totalRow] };
}
