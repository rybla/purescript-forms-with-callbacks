import fs from "fs/promises";
import path from "path"

const pages_dirpath = "page"

const page_filepaths =
    (await fs.readdir(pages_dirpath))
        .map((filename) => path.join(pages_dirpath, filename))

await Bun.build({
    entrypoints: page_filepaths,
    outdir: "client",
    minify: true,
})
