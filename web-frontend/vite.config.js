import { defineConfig } from "vite";

export default defineConfig({
  server: {
    port: 5173,
    proxy: {
      "/trace": "http://localhost:8125",
    },
  },
  build: {
    outDir: "vite-dist",
    emptyOutDir: true,
  },
});
