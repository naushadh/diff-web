# diff-web

A proxy to A/B test web services.

A common pattern to test web services is to deploy a copy of the server with the new code under a different host/ELB while the baseline/control instance is left untouched. During this time, various manual and automated tests are run to ensure the new instance of the service meets QA acceptance.

While some changes are expected (addition of new fields, removal of deprecated ones), most day to day bug fixes and under the hood improvements are not expected to change the final output of the service. `diff-web` is a proxy that forwards your requests to 2 endpoints before diff-ing the response body.

## Installation

NOTE: We require the [`diff`](http://man7.org/linux/man-pages/man1/diff.1.html) command to be available in the `$PATH`.

### Build from source

- Get [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

- Clone this repository

  ```bash
  git clone git@github.com:naushadh/diff-web.git
  ```

- From repository folder, build

  ```bash
  stack build --pedantic --ghc-options '-O2'
  ```

- Install

  ```bash
  stack install diff-web-exe
  ```

## Usage

### Settings

The following settings can be set via ENV

```env
DIFF_PORT=8080
```

### Launch

```bash
$ diff-web-exe
... should print port listened on
```

### Proxy requests

- Send all desired requests to the app

- Ensure the following headers are set for forward endpoints

  ```txt
  DIFF_HOST_A: app.com
  DIFF_HOST_B: dev.app.com
  ```

- When responses are the same, you get the response from host B

- When responses differ, you get a 400 error with the diff in the body.

## TODO

In no particular order

- [ ] Add a simple HTML UI for better user-friendliness (esp: very large diffs).
- [ ] Add tests
- [ ] Add Dockerfile for build+deployment convenience
- [ ] Find a pure haskell `diff` alternative and/or ship it with the app to ensure we're fully self contained.
