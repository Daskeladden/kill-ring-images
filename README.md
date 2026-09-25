# kill-ring-images

Images in the Emacs kill ring.

With `kill-ring-images-mode` on, an image that another program copies
joins the kill ring at your next yank, next to text. `M-y`, `yank-pop`
and `consult-yank-pop` show it as a short entry such as
`[image 240x120 png 3f9a1c]`. On X, Emacs also offers an image that it
copied to other programs.

The built-in `yank-media` pastes an image from the clipboard into a
buffer that has a handler for it. kill-ring-images keeps the image in
the kill ring, so a later yank can still take it, and a yank of it
uses the same handlers.

## Requirements

- Emacs 29.1 or later.
- X, for offering images to other programs. On other window systems,
  a yank takes clipboard images where Emacs can read image data from
  the clipboard.

Emacs can show on another machine, for example over SSH X forwarding.
Then the X server there must carry images between the X clipboard and
the clipboard of that machine.

## Install

With elpaca:

```elisp
(use-package kill-ring-images
  :ensure (:host github :repo "Daskeladden/kill-ring-images")
  :demand t
  :config
  (kill-ring-images-mode 1))
```

Or clone it:

```elisp
(add-to-list 'load-path "~/src/kill-ring-images")
(require 'kill-ring-images)
(kill-ring-images-mode 1)
```

## Copy

`kill-ring-images-copy-image-at-point` copies the image under point, in
any buffer and in `image-mode`. A copy offers the image in its own type:
PNG, JPEG, GIF or SVG.

Any copy that holds nothing but an image becomes the same kind of
entry, whatever command made it. `M-w` on a line that shows only an
image, or on a marked image, is enough.

## Yank

A yank of an image does the first of these that applies.

| Buffer | Result |
|---|---|
| A buffer with a yank-media handler, such as html-mode, message-mode, or org with Org 9.7 or later | The handler gets the image |
| markdown-mode | The image is written to `images/` next to the file, and a link is inserted |
| ghostel or vterm | The terminal gets the path of the image file |
| Any other buffer | The image is shown inline |

When the clipboard offers text and an image together, the text wins.

Org and markdown-mode show an image link as text until you ask for a
preview. After a yank puts an image link in such a buffer, the yank
shows the preview at once. In org, the yank uses the rule of Org's own
`C-c C-x C-v`: a link with a description gets no preview. Org's
attachment links have a description, so `C-1 C-c C-x C-v` shows them.
To turn the preview off, set `kill-ring-images-yank-preview` to nil.

## For other packages

| Name | What it is for |
|---|---|
| `kill-ring-images-copy` | A function of a MIME type and bytes that puts an image in the kill ring and on the clipboard |
| `kill-ring-images-copy-functions` | A hook whose functions also get each image that Emacs copies, with its MIME type and bytes |
| `kill-ring-images-image-functions` | A hook whose functions give an image that the X clipboard does not offer, as `(TYPE . BYTES)` |
| `kill-ring-images-copy-to-x` | A package that carries copies to the clipboard in another way sets it to nil |
| `kill-ring-images-yank-terminal-paste-functions` | A hook run after an image went to a terminal as a file path, with the buffer and the image id |
| `kill-ring-images-entry-thumbnail` | A small image of an entry, for a kill ring browser that draws one beside the text |

## Options

| Option | Default | Meaning |
|---|---|---|
| `kill-ring-images-store-limit` | 64 MiB | The total size of the images kept in memory. The oldest go first. |
| `kill-ring-images-types` | png, jpeg, gif, svg | The image types, in the order of preference |
| `kill-ring-images-preview-lines` | 5 | The height of an image in the kill ring, in text lines |
| `kill-ring-images-file-directory` | `temporary-file-directory` | Image files for terminals and unsaved markdown buffers |
| `kill-ring-images-markdown-directory` | `"images"` | Image files next to a markdown file |
| `kill-ring-images-yank-preview` | t | Whether a yank into org or markdown-mode shows the image link it inserts as a picture |

## Tests

```sh
make test      # the unit tests, in batch
make compile   # byte-compile with warnings as errors
make test-x    # the round trip through a real X server, needs xvfb-run and xclip
```

`make test-x` runs a graphical Emacs on a display of its own. It never
touches the display that you work on.

## License

GPL-3.0-or-later. See `LICENSE`.
