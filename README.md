# Lego

WIP SQL generator for OCaml & Reason. Check out `bin/Test.re` for examples.

## Usage

You need Esy, you can install the beta using [npm][]:

    % yarn global add esy@latest

Then you can install the project dependencies using:

    % esy install

Then build the project dependencies along with the project itself:

    % esy build

After you make some changes to source code, you can re-run project's build
using:

    % esy build

And test compiled executable:

    % esy ./_build/default/bin/Test.exe

Shell into environment:

    % esy shell
