## **yaai & yaai_ui – Open-Source AI Tools for ABAP Developers**

A few months ago, I embarked on a journey to learn about and explore large language models. After spending some time coding in Python and experimenting with Ollama, LangChain, LangGraph, and many other tools in the Python ecosystem, I decided to challenge myself to do similar things in ABAP, the programming language I've been working with for nearly two decades.

Well, I'm still on this journey, and to be honest, as crazy as it might sound, I'm having a lot of fun. I've lost count of how many times I've found myself laughing, hardly believing what I was seeing while testing features like function calling and retrieval-augmented generation. Honestly, developing AI-powered applications in ABAP is something that never crossed my mind. For someone who started with ABAP version 4.6C, it's quite mind-blowing.

I think the tools I've been developing over the last few months might be of interest to other ABAP developers who want to dive into the rabbit hole of large language models and all the things they are capable of. The possibilities are endless.

So, it's time to share this with the ABAP community. I hope to see many of you having fun using AI right within ABAP, bringing new ideas, and hopefully contributing to these projects. 😊

## **Introducing `yaai` & `yaai_ui`**

I'm happy to share **yaai - ABAP AI tools** and its companion project, **yaai_ui - ABAP AI User Interface** with the ABAP community! These two open-source repositories are designed to equip ABAP developers to easily integrate Artificial Intelligence, specifically Large Language Models (LLMs), directly into their SAP systems using **pure ABAP code**.

The goal of yaai and yaai_ui is to enable ABAP developers to learn and explore Artificial Intelligence capabilities within their familiar environment—at no cost. The only potential cost is for paid AI APIs (such as OpenAI), if you choose to use them. You can get started right away using free APIs like Google Gemini, or by running your own self-hosted LLMs with Ollama.

### **Introducing yaai - ABAP AI tools: Your Backend AI Engine for ABAP**

The `christianjianelli/yaai` repository provides a set of tools focused on backend AI integration for ABAP environments. It's built to facilitate powerful AI capabilities that you can leverage directly within your SAP applications.

**Key Features of ABAP AI tools include:**

*   **Multi-Provider LLM API Support**: This is a core strength, offering flexibility in choosing your LLM provider and deployment model. Currently supported APIs include **OpenAI (cloud-based)**, **Ollama (local/self-hosted)**, and **Google Gemini (cloud-based)**. The tools are also compatible with APIs from other providers, such as Nvidia, that implement OpenAI API standards, possibly with minor adjustments.
*   **Conversation Management**: The tools allow you to maintain and manage multi-turn conversations, providing full access to conversation history for advanced chat scenarios directly within your ABAP applications.
*   **Tool/Function Calling**: Integrate your existing **ABAP business logic with LLMs** using function/tool calling. This enables AI models to directly trigger ABAP methods, opening up new possibilities for intelligent automation and process enhancement.
*   **Retrieval-Augmented Generation (RAG)**: Enhance the accuracy and relevance of LLM outputs by incorporating your **enterprise data**. This allows you to build applications that provide more context-aware and specific answers by pulling information from your SAP system.

These features collectively can help you to learn how to build **intelligent ABAP applications** that leverage the latest advancements in AI.

### **Meet yaai_ui - ABAP AI User Interface: Bringing AI to the User's Fingertips**

Complementing the backend tools, the `christianjianelli/yaai_ui` repository provides ready-to-use user interface components to interact with LLMs directly from your SAP system. This project simplifies the creation of engaging AI-powered frontends in ABAP.

**The ABAP AI User Interface includes:**

*   **Chat Interface**: Enable conversational AI interactions directly within your SAP system, allowing users to engage with LLMs in a familiar chat window. 
*   **Code Assistant**: Access **AI-powered code suggestions and assistance for ABAP development**, potentially boosting developer productivity and code quality.

### **Getting Started is Simple!**

Both repositories can be easily installed into your SAP system using **abapGit**.

**Prerequisites for installation include:**

*   An SAP system running **ABAP version 7.52 or higher**.
*   **abapGit** installed and configured in your ABAP system.
*   Appropriate **developer authorizations** in your ABAP system.

**Important Note**: To use the ABAP AI User Interface, the **ABAP AI Tools (`yaai`) must be installed first** in your SAP system. Detailed installation steps are provided in each repository's documentation.

You can find quickstart guides in the documentation to help you run your first simple LLM application or chat application.

### **Explore, Experiment, and Contribute!**

These tools are currently **experimental** and released under the **MIT License**. This means they are provided "as is" and you use them at your own risk. However, this also means they are open for community collaboration!

I encourage you to:

*   **Explore** the repositories on GitHub:
    *   **yaai - ABAP AI tools**: `https://github.com/christianjianelli/yaai`
    *   **yaai_ui - ABAP AI User Interface**: `https://github.com/christianjianelli/yaai_ui`
*   **Experiment** with the examples and integrate them into your own ABAP projects.
*   Provide **feedback** and **contribute** to the evolving project.

Start building your intelligent ABAP applications today! 😊

*This text was written with the help of Artificial Intelligence.*

---