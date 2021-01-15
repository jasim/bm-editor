## Setup

```sh
npm install
npm run clean; npm run start
npx webpack -w
open localhost:4500
```

## Further dev

- Mouse

- Text mode editor

- Line highlight

- Line numbers

- Scroll

- Soft-wrap

- Unicode

- Multicursor

- Mobile devices

- Live Markdown

- WebGL

- Native layoutting + hit-testing with position: absolute.
  (https://github.com/pixijs/pixi.js/blob/dev/packages/interaction/src/TreeSearch.ts)

- Cross-browser support (for eg:
  https://stackoverflow.com/questions/4194163/detect-printable-keys/38802011#comment89017864_38802011)

# Notes

### Selection: Anchor, Start/End

https://draftjs.org/docs/api-reference-selection-state/

> The SelectionState therefore exposes both anchor/focus values and start/end values. When managing selection behavior, we recommend that you work with anchor and focus values to maintain selection direction. When managing content operations, however, we recommend that you use start and end values.
