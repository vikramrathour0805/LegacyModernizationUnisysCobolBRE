# Unisys COBOL Analyzer

A comprehensive web-based tool for analyzing Unisys COBOL code, generating Abstract Syntax Trees (AST), extracting grammar patterns, and providing AI-powered Q&A capabilities for code understanding.

## Features

### 🚀 Core Functionality
- **Multi-file Upload**: Drag-and-drop interface for uploading single or multiple .cbl, .cob, and .cobol files
- **COBOL Parser**: Advanced parser specifically designed for Unisys COBOL dialect
- **AST Generation**: Complete Abstract Syntax Tree generation with hierarchical code structure
- **Grammar Extraction**: Automatic extraction of COBOL grammar rules and patterns
- **Documentation Generation**: Automated documentation of processes, data flows, and functionalities

### 🤖 AI-Powered Analysis
- **Q&A Interface**: Natural language questions about your COBOL code
- **Code Understanding**: AI-powered insights into program functionality, data structures, and flow control
- **Performance Analysis**: Identification of potential performance issues and optimization recommendations
- **Dependency Mapping**: Analysis of external and internal dependencies

### 📊 Comprehensive Analysis
- **Data Structure Analysis**: Complete breakdown of data items, levels, and relationships
- **Call Graph Generation**: Visual representation of procedure calls and relationships
- **Data Flow Tracking**: Analysis of data movement and transformations
- **Control Flow Analysis**: Understanding of conditional logic and program flow

## Technical Architecture

### Frontend
- **Next.js 14**: React framework with App Router
- **TypeScript**: Type-safe development
- **Tailwind CSS**: Modern, responsive UI styling
- **Lucide React**: Beautiful, consistent icons
- **React Dropzone**: File upload functionality

### Backend
- **Next.js API Routes**: Server-side processing
- **Custom COBOL Parser**: Tailored for Unisys COBOL dialect
- **AI Integration**: Intelligent code analysis and Q&A

## Getting Started

### Prerequisites
- Node.js 18+ 
- npm or yarn

### Installation

1. Clone the repository:
```bash
git clone <repository-url>
cd cobol-analyzer
```

2. Install dependencies:
```bash
npm install
```

3. Start the development server:
```bash
npm run dev
```

4. Open [http://localhost:3000](http://localhost:3000) in your browser

## Usage

### 1. Upload COBOL Files
- Navigate to the "Upload Files" tab
- Drag and drop .cbl, .cob, or .cobol files into the upload area
- Or click to select files manually
- Click "Analyze COBOL Files" to start processing

### 2. View Analysis Results
- Switch to "Analysis Results" tab after processing
- Download AST and Grammar files as JSON
- Explore program structure, data items, and call graphs
- Review generated documentation

### 3. Ask Questions
- Use the "Q&A Interface" tab to ask questions about your code
- Try predefined questions or ask custom queries
- Get AI-powered insights about functionality, structure, and optimization

### 4. Generate Documentation
- View automatically generated documentation
- Understand processes, data flows, and functionalities
- Export results for further analysis

## COBOL Dialect Support

This analyzer is specifically optimized for **Unisys COBOL** dialect, supporting:

### Language Features
- ✅ All four divisions (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- ✅ Working Storage Section data definitions
- ✅ Complex data structures with OCCURS and REDEFINES
- ✅ Procedure division paragraphs and sections
- ✅ MOVE, PERFORM, IF, EVALUATE statements
- ✅ Arithmetic operations (ADD, SUBTRACT, MULTIPLY, DIVIDE)
- ✅ String manipulation (STRING, UNSTRING)
- ✅ Display and Accept operations
- ✅ Computational data types (COMP, COMP-3, BINARY)

### Analysis Capabilities
- **Syntax Parsing**: Complete parsing of COBOL source code
- **Semantic Analysis**: Understanding of data relationships and program flow
- **Pattern Recognition**: Identification of common COBOL patterns and idioms
- **Error Detection**: Syntax and semantic error identification
- **Warning Generation**: Best practice recommendations

## API Endpoints

### POST /api/analyze
Analyzes uploaded COBOL files and returns AST, grammar, and documentation.

**Request**: FormData with files
**Response**: Array of AnalysisResult objects

### POST /api/qa
Processes natural language questions about analyzed COBOL code.

**Request**: JSON with question, analysisResults, and context
**Response**: QAResponse with answer, confidence, and sources

## File Structure

```
cobol-analyzer/
├── src/
│   ├── app/
│   │   ├── api/
│   │   │   ├── analyze/route.ts
│   │   │   └── qa/route.ts
│   │   └── page.tsx
│   ├── components/
│   │   ├── FileUploader.tsx
│   │   ├── AnalysisResults.tsx
│   │   └── QAInterface.tsx
│   ├── lib/
│   │   ├── cobol-parser.ts
│   │   ├── documentation-generator.ts
│   │   └── qa-engine.ts
│   └── types/
│       └── analysis.ts
├── public/
└── package.json
```

## Example Usage

### Sample COBOL Code Analysis

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. SAMPLE-PROGRAM.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-COUNTER    PIC 9(3) VALUE 0.
01  WS-NAME       PIC X(30).

PROCEDURE DIVISION.
0000-MAIN-LOGIC.
    PERFORM 1000-INITIALIZE.
    PERFORM 2000-PROCESS-DATA.
    STOP RUN.

1000-INITIALIZE.
    MOVE 0 TO WS-COUNTER.
    DISPLAY "Program Started".

2000-PROCESS-DATA.
    ACCEPT WS-NAME.
    DISPLAY "Hello " WS-NAME.
```

### Generated AST Structure
```json
{
  "fileName": "sample.cbl",
  "programId": "SAMPLE-PROGRAM",
  "sections": [...],
  "dataItems": [
    {
      "level": 1,
      "name": "WS-COUNTER",
      "picture": "9(3)",
      "value": "0"
    }
  ],
  "paragraphs": [...],
  "callGraph": {
    "0000-MAIN-LOGIC": ["1000-INITIALIZE", "2000-PROCESS-DATA"]
  },
  "dataFlows": [...]
}
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

## License

MIT License - see LICENSE file for details

## Support

For issues and questions:
- Create an issue on GitHub
- Check the documentation
- Review the example files

---

**Built with ❤️ for COBOL developers and legacy system modernization**
