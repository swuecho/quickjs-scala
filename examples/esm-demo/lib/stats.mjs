// Small statistics helpers, exported as named and default exports.

export const sum = (values) => values.reduce((acc, value) => acc + value, 0);

export const mean = (values) => sum(values) / values.length;

export function median(values) {
  const sorted = [...values].sort((a, b) => a - b);
  const middle = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 0 ? (sorted[middle - 1] + sorted[middle]) / 2 : sorted[middle];
}

export function stddev(values) {
  const average = mean(values);
  const variance = values.reduce((acc, value) => acc + (value - average) ** 2, 0) / values.length;
  return Math.sqrt(variance);
}

// Default export: a one-line human-readable description.
export default function describe(values) {
  return `n=${values.length} sum=${sum(values)} mean=${mean(values).toFixed(1)} median=${median(values)} stddev=${stddev(values).toFixed(1)}`;
}
