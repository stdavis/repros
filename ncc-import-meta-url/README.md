This is a minimal example of an issue that I'm experiencing with ncc and `import.meta.url`.

<https://github.com/vercel/ncc/issues/1200>

### Steps to reproduce

1. Verify that the source runs correctly: `node index.js`
1. Run the ncc build: `npx ncc build index.js`
1. Run the built file: `node dist/index.js`
