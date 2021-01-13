open Belt
@bs.val external document: {..} = "document"
@bs.val external window: {..} = "window"

module Editor = {
  type cursor = {
    x: int,
    y: int,
  }

  type t = {
    cursor: cursor,
    text: array<string>,
  }

  let cursorSegmented = t => {
    let l = t.text[t.cursor.y]->Option.getWithDefault("")
    let before = l |> Js.String.substring(~from=0, ~to_=t.cursor.x)
    let after = l |> Js.String.substr(~from=t.cursor.x)
    (before, after)
  }

  let toHtmlString = t => {
    t.text->Array.mapWithIndex((i, l) => {
      if i == t.cursor.y {
        let (beforeCursor, afterCursor) = cursorSegmented(t)
        "<p class='line'><span>" ++
        beforeCursor ++
        "</span><span id='cursor'></span><span>" ++
        afterCursor ++ "</span></p>"
      } else {
        "<p class='line'>" ++ l ++ "</p>"
      }
    }) |> Js.Array.joinWith("\n")
  }

  let render = (t, dom) => {
    dom["innerHTML"] = toHtmlString(t)
  }

  let handleEvent = (tRef, dom, event) => {
    let t = tRef.contents
    let i = event["keyCode"]
    let letter = event["key"]
    let isContentKey = {
      /* https://stackoverflow.com/a/38802011 */
      Js.String.length(letter) == 1
    }
    if isContentKey {
      let (before, after) = cursorSegmented(t)
      {t.text[t.cursor.y] = before ++ letter ++ after} |> ignore
      tRef := {...t, cursor: {...t.cursor, x: t.cursor.x + 1}}
      render(tRef.contents, dom)
    }
  }
}

/*
- Get event
  - if printable, add it to current cursor position.
  - if arrows, move
  - if large movement, move
  - if delete/bkspc, delete
  - if enter, insert line
 */

let init = editorDom => {
  let state: ref<Editor.t> = {
    contents: {
      cursor: {x: 3, y: 0},
      text: ["Hello,", "This is a long line."],
    },
  }

  editorDom["addEventListener"]("keydown", event => {
    Editor.handleEvent(state, editorDom, event)
  }) |> ignore

  editorDom["focus"]() |> ignore
  Editor.render(state.contents, editorDom)
}

let start = () => {
  let editorDom = document["getElementById"]("editor")
  Js.log("Hello!")
  init(editorDom)
}

window["onload"] = start
