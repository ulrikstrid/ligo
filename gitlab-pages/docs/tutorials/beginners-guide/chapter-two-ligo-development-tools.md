---
id: tezos-beginners-guide-chapter-two-ligo-dev-tools
title: Chapter 2 - LIGO Development Tools
---

## Installing LIGO
Currently the best way to install LIGO is to use LIGO's pre-baked docker image. Please refer to [here](https://docs.docker.com/install/) for instructions on how to install docker. 

```
curl https://gitlab.com/ligolang/ligo/raw/dev/scripts/installer.sh | bash -s "next"
```

## Editors
The recommended editor to develop with LIGO is [vscode](https://code.visualstudio.com/). This is because the LIGO team has added syntax highlighting in vscode for the PascaLIGO syntax. The LIGO team plans to support developer tools such as intellisense and additional syntax support. 

To install the PascaLIGO vscode plugin open up vscode and hit the extensions icon on the left toolbar. Then search for LIGO and install the plugin published by the *LIGO* Team.

<img src="/img/tutorials/beginners-guide/vscode-ligo.png" />

## Additional Tools
If you embedding PascaLIGO code in your website, we have also added syntax highlighting support in the popular language library [Prism](https://github.com/PrismJS/prism). 