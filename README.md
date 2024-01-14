# Neurochat: GUI for LLMs

## Description
Neurochat is a 100% native front-end application designed to facilitate interaction with various AI services, including the Neuroengine service, OpenAI's ChatGPT API, and local AI models using the [Llama.cpp](https://github.com/ggerganov/llama.cpp) library. Its primary function is to offer a cohesive user experience while managing these diverse AI systems through one simple and convenient GUI. Installation provided for [Linux](https://github.com/ortegaalfredo/neurochat/releases/download/0.6-dev/neurochat-0.6-amd64-ubuntu.deb), [Windows](https://github.com/ortegaalfredo/neurochat/releases/download/0.6-dev/neurochat-0.6-setup-win64.exe) and [MacOS](https://github.com/ortegaalfredo/neurochat/releases/download/0.3-dev/neurochat-0.3-setup-macOS-aarm64.pkg) support (MacOS still quite buggy).

![Neurochat demo](https://raw.githubusercontent.com/ortegaalfredo/neurochat/main/neurochat-demo.gif)

## Basic usage
You have several options to access AIs:

1. Neuroengine.ai: This service provides unrestricted access to a variety of general-purpose, open-source AI models without requiring user registration. Contrasting with many other AIs, these models typically remain unmoderated. It's important to note that advanced AI systems often take longer to generate responses compared to their less complex counterparts.

2. ChatGPT API: Use ChatGPT via the OpenAI API. Several models are available, including ChatGPT 3.5 and ChatGPT4-turbo.

3. Open Local AI: Open and use a local AI stored in your hard disk using LLama.cpp module. This requires fast hardware and enought RAM to fit the AI in memory but it allows a level of privacy and customization that other services cannot provide. Additionally, this can be used without any internet connection, but you must download the AI files to your local HDD.

## Recommended local AIs (as of Dec 2023)

If you choose to run a Local AI, you need to download the AI neural network to your local disk. The AI files are usually several gigabytes in lenght. Recommended AIs are:

* Mistral-7B This is a small free AI from Mistral.ai, that still have good quality. It requires 5 GB of disk and 8GB of RAM memory. A good version of this AI is [Mistral-7B](https://huggingface.co/TheBloke/mistral-7B-finetuned-orca-dpo-v2-GGUF/blob/main/mistral-7b-finetuned-orca-dpo-v2.Q4_K_M.gguf) from TheBloke repository.

* Mixtral-8x7B This is a large free AI from Mistral.ai, excellent quality rivaling ChatGPT 3.5. It requires 20 GB of disk and 24GB of RAM memory. Mixtral is slower but higher quality than Mistral. It can be downloaded from [here](https://huggingface.co/TheBloke/Mixtral-8x7B-Instruct-v0.1-GGUF/resolve/main/mixtral-8x7b-instruct-v0.1.Q3_K_M.gguf)

## Using ChatGPT API

You can use ChatGPT 3.5, ChatGPT4 and any chat model from OpenAI, directly from the Neurochat app, just following those steps:

1. Register with OpenAI to acquire an API key. Note that OpenAI may charge for this service.

2. After obtaining the API key from OpenAI, insert it into the “options” window, under the “ChatGPT API” tab.

3. In the 'Connect AI' menu from the right button or the main menu, choose 'ChatGPT API' and then the model you want.

One advantage of using OpenAI API for ChatGPT instead of their free web version, is their assurance that your data is not utilized for training Language Learning Models (LLMs). Another benefit is gaining access to GPT4 without purchasing the GPT+ subscription - simply pay per individual query. Note that Neurochat authors are not associated with OpenAI and do not receive any kind of compensation.

## Personality

You can set the personality of the AI on each individual Tab. There are several personalities available from "Assistant" (the default) to a Coder personality based on Terry Davis, and you can also create your own personality in the "Options" windows and seeting the personality as "Custom". After setting a new personality, it is recommended to reset the conversation.

## Additional features

The Neurochat app will save and load automatically all tabs. Also you can export the chat contents of a single tab in html or txt format using the "Save chat as..." menu.

## Build instructions

You need Lazarus 3.0.0. Some components need to be installed into the ide:

1. THTMLViewer
2. fpc-llama
3. fpc-markdown-1.1.1
4. openai-delphi

Those components are under the 'component' directory.

After this, only issue:

```lazbuild neurochat.lpi```

This should work on Windows, Linux and OS-X.

You will need llama.cpp compiled as a shared library named libllama.so on Linux, libllama.dylib on macOS, and llama.dll on Windows. Just put the library on the project directory so Lazarus can find it when building the project.

## GPU acceleration

The GUI has support for activating GPU support in llama.cpp, but the shared library need to be compiled with GPU support enabled. 
Currently, the packaged version on this github only supports CPU AVX2 acceleration. If you want to use CUDA acceleration, you must download a proper llama.cpp GPU-enabled binary from [here](https://github.com/ggerganov/llama.cpp/releases/tag/b1696)


