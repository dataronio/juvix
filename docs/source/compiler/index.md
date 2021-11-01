# Compiler


<!-- https://q.uiver.app/?q=WzAsMTksWzAsMCwiXFx0ZXh0e0p1dml4IGZpbGV9Il0sWzIsNSwiXFx0ZXh0e0NvcmV9Il0sWzMsNywiXFx0ZXh0e0NvcmV9Il0sWzEsNywiXFx0ZXh0e0V2YWx9Il0sWzEsOSwiXFx0ZXh0e0xMVk0gfCBNaWNoZWxzb24gfCBQbG9ua30iXSxbMiwwLCJcXHRleHR7QVNUfSJdLFs4LDAsIlxcdGV4dHtEZXN1Z2FyZWR9Il0sWzgsMSwiXFxidWxsZXQiXSxbOCwyLCJcXGJ1bGxldCJdLFs4LDMsIlxcYnVsbGV0Il0sWzgsNCwiXFxidWxsZXQiXSxbOCw1LCJcXGJ1bGxldCJdLFs3LDUsIlxcYnVsbGV0Il0sWzUsNSwiXFxidWxsZXQiXSxbNCw1LCJcXGJ1bGxldCJdLFs3LDcsIlxcYnVsbGV0Il0sWzgsNywiXFxidWxsZXQiXSxbNyw4LCJcXGJ1bGxldCJdLFs4LDgsIlxcYnVsbGV0Il0sWzAsNSwicGFyc2UiLDAseyJjb2xvdXIiOlsyNDAsNjAsNjBdfSxbMjQwLDYwLDYwLDFdXSxbNiwxLCJlbGFib3JhdGUiLDEseyJjb2xvdXIiOlsyNDAsNjAsNjBdfSxbMjQwLDYwLDYwLDFdXSxbNiw3LCJcXHRleHRpdHtjb250ZXh0aWZ5fSIsMCx7ImNvbG91ciI6WzAsMCw1MF19LFswLDAsNTAsMV1dLFs3LDgsIlxcdGV4dGl0e25hbWVkIHZhcmlhYmxlc30iLDAseyJjb2xvdXIiOlswLDAsNTBdfSxbMCwwLDUwLDFdXSxbOCw5LCJcXHRleHRpdHt0YWN0aWNzfSIsMCx7ImNvbG91ciI6WzAsMCw1MF19LFswLDAsNTAsMV1dLFs5LDEwLCJcXHRleHRpdHtwYXR0ZXJuIG1hdGNoaW5nfSIsMCx7ImNvbG91ciI6WzAsMCw1MF19LFswLDAsNTAsMV1dLFsxMCwxMSwiXFx0ZXh0aXR7aG9sZXN9IiwwLHsiY29sb3VyIjpbMCwwLDUwXX0sWzAsMCw1MCwxXV0sWzExLDEyLCJcXHRleHRpdHtTY29wZSBjaGVja2luZ30iLDAseyJjb2xvdXIiOlswLDAsNTBdfSxbMCwwLDUwLDFdXSxbMTIsMTMsIlxcdGV4dGl0e3R5cGUgY2hlY2tpbmd9IiwwLHsiY29sb3VyIjpbMCwwLDUwXX0sWzAsMCw1MCwxXV0sWzEsMCwiXFxtYXRoc2Z7ZGVzdGlsYXRpb259IiwxLHsiY29sb3VyIjpbMCwwLDUwXSwic3R5bGUiOnsiYm9keSI6eyJuYW1lIjoiZGFzaGVkIn19fSxbMCwwLDUwLDFdXSxbMSwyLCJcXG1hdGhzZntDUFN9IiwxLHsiY3VydmUiOi0yLCJjb2xvdXIiOlswLDAsNTBdfSxbMCwwLDUwLDFdXSxbMSwyLCJcXG1hdGhzZntBTkZ9IiwxLHsiY3VydmUiOjIsImNvbG91ciI6WzAsMCw1MF19LFswLDAsNTAsMV1dLFsxNCwxLCJob2xlc1xcIHNvbHZlciIsMCx7ImNvbG91ciI6WzAsMCw1MF19LFswLDAsNTAsMV1dLFsxLDMsImVyYXNlIiwxLHsiY29sb3VyIjpbMjQwLDYwLDYwXX0sWzI0MCw2MCw2MCwxXV0sWzIsMywiZXJhc2UiLDEseyJjb2xvdXIiOlsyNDAsNjAsNjBdfSxbMjQwLDYwLDYwLDFdXSxbMyw0LCJjb21waWxlIiwxLHsiY29sb3VyIjpbMjQwLDYwLDYwXX0sWzI0MCw2MCw2MCwxXV0sWzE1LDE2LCJtYWluXFwgc3RlcCIsMCx7ImNvbG91ciI6WzI0MCw2MCw2MF19LFsyNDAsNjAsNjAsMV1dLFsxNywxOCwic2Vjb25kYXJ5XFwgc3RlcCIsMCx7ImNvbG91ciI6WzAsMCw1MF19LFswLDAsNTAsMV1dLFs1LDYsImRlc3VnYXIiLDAseyJjb2xvdXIiOlsyNDAsNjAsNjBdfSxbMjQwLDYwLDYwLDFdXSxbMTMsMTQsIlxcbWF0aHNme3VuaWZpY2F0aW9ufSIsMCx7ImNvbG91ciI6WzAsMCw1MF19LFswLDAsNTAsMV1dXQ== -->
<iframe class="quiver-embed" src="https://q.uiver.app/?q=WzAsMTksWzAsMCwiXFx0ZXh0e0p1dml4IGZpbGV9Il0sWzIsNSwiXFx0ZXh0e0NvcmV9Il0sWzMsNywiXFx0ZXh0e0NvcmV9Il0sWzEsNywiXFx0ZXh0e0V2YWx9Il0sWzEsOSwiXFx0ZXh0e0xMVk0gfCBNaWNoZWxzb24gfCBQbG9ua30iXSxbMiwwLCJcXHRleHR7QVNUfSJdLFs4LDAsIlxcdGV4dHtEZXN1Z2FyZWR9Il0sWzgsMSwiXFxidWxsZXQiXSxbOCwyLCJcXGJ1bGxldCJdLFs4LDMsIlxcYnVsbGV0Il0sWzgsNCwiXFxidWxsZXQiXSxbOCw1LCJcXGJ1bGxldCJdLFs3LDUsIlxcYnVsbGV0Il0sWzUsNSwiXFxidWxsZXQiXSxbNCw1LCJcXGJ1bGxldCJdLFs3LDcsIlxcYnVsbGV0Il0sWzgsNywiXFxidWxsZXQiXSxbNyw4LCJcXGJ1bGxldCJdLFs4LDgsIlxcYnVsbGV0Il0sWzAsNSwicGFyc2UiLDAseyJjb2xvdXIiOlsyNDAsNjAsNjBdfSxbMjQwLDYwLDYwLDFdXSxbNiwxLCJlbGFib3JhdGUiLDEseyJjb2xvdXIiOlsyNDAsNjAsNjBdfSxbMjQwLDYwLDYwLDFdXSxbNiw3LCJcXHRleHRpdHtjb250ZXh0aWZ5fSIsMCx7ImNvbG91ciI6WzAsMCw1MF19LFswLDAsNTAsMV1dLFs3LDgsIlxcdGV4dGl0e25hbWVkIHZhcmlhYmxlc30iLDAseyJjb2xvdXIiOlswLDAsNTBdfSxbMCwwLDUwLDFdXSxbOCw5LCJcXHRleHRpdHt0YWN0aWNzfSIsMCx7ImNvbG91ciI6WzAsMCw1MF19LFswLDAsNTAsMV1dLFs5LDEwLCJcXHRleHRpdHtwYXR0ZXJuIG1hdGNoaW5nfSIsMCx7ImNvbG91ciI6WzAsMCw1MF19LFswLDAsNTAsMV1dLFsxMCwxMSwiXFx0ZXh0aXR7aG9sZXN9IiwwLHsiY29sb3VyIjpbMCwwLDUwXX0sWzAsMCw1MCwxXV0sWzExLDEyLCJcXHRleHRpdHtTY29wZSBjaGVja2luZ30iLDAseyJjb2xvdXIiOlswLDAsNTBdfSxbMCwwLDUwLDFdXSxbMTIsMTMsIlxcdGV4dGl0e3R5cGUgY2hlY2tpbmd9IiwwLHsiY29sb3VyIjpbMCwwLDUwXX0sWzAsMCw1MCwxXV0sWzEsMCwiXFxtYXRoc2Z7ZGVzdGlsYXRpb259IiwxLHsiY29sb3VyIjpbMCwwLDUwXSwic3R5bGUiOnsiYm9keSI6eyJuYW1lIjoiZGFzaGVkIn19fSxbMCwwLDUwLDFdXSxbMSwyLCJcXG1hdGhzZntDUFN9IiwxLHsiY3VydmUiOi0yLCJjb2xvdXIiOlswLDAsNTBdfSxbMCwwLDUwLDFdXSxbMSwyLCJcXG1hdGhzZntBTkZ9IiwxLHsiY3VydmUiOjIsImNvbG91ciI6WzAsMCw1MF19LFswLDAsNTAsMV1dLFsxNCwxLCJob2xlc1xcIHNvbHZlciIsMCx7ImNvbG91ciI6WzAsMCw1MF19LFswLDAsNTAsMV1dLFsxLDMsImVyYXNlIiwxLHsiY29sb3VyIjpbMjQwLDYwLDYwXX0sWzI0MCw2MCw2MCwxXV0sWzIsMywiZXJhc2UiLDEseyJjb2xvdXIiOlsyNDAsNjAsNjBdfSxbMjQwLDYwLDYwLDFdXSxbMyw0LCJjb21waWxlIiwxLHsiY29sb3VyIjpbMjQwLDYwLDYwXX0sWzI0MCw2MCw2MCwxXV0sWzE1LDE2LCJtYWluXFwgc3RlcCIsMCx7ImNvbG91ciI6WzI0MCw2MCw2MF19LFsyNDAsNjAsNjAsMV1dLFsxNywxOCwic2Vjb25kYXJ5XFwgc3RlcCIsMCx7ImNvbG91ciI6WzAsMCw1MF19LFswLDAsNTAsMV1dLFs1LDYsImRlc3VnYXIiLDAseyJjb2xvdXIiOlsyNDAsNjAsNjBdfSxbMjQwLDYwLDYwLDFdXSxbMTMsMTQsIlxcbWF0aHNme3VuaWZpY2F0aW9ufSIsMCx7ImNvbG91ciI6WzAsMCw1MF19LFswLDAsNTAsMV1dXQ==&embed" width="100%" height="500" style="border-radius: 8px; border: none;"></iframe>

-----------

- [Frontend](./frontend/index.md)


  - **Parse**:
      Break up Juvix syntax into an AST
  
  - **Desugar**:
      Translate structure to compiler-friendly terms
      - *Serialise*:
      Convert AST to S-expression
      - *Desugar S-Expressions*:
      Strip away high-level constructs from the source (see [s-expression syntax](./frontend/s-expression-syntax))
      - *Deserialise*"
      Convert S-expression back to AST
    
- [Core](./core/index.md)
  - **Elaboration** 
      - **Contextify** 
        - *Resolve Open*:
      Construct a map of terms to the modules where they were defined
        - *Resolve Open In*
      
        - *Qualify Names*:
      Replace term names with their qualified names
      
        - *Resolve Infix*:
      Shunting-yard algorithm. Needs context for infixivity
      
        - *Lookup Record Fields*
      
      - Implicit arguments
      - Named variables
      - Tactics
      - Pattern matching for function declaration
      - Holes
      - Scope checking
      - Type checking
      - Unification
      - $\beta \eta$ - conversion checking
      - Holes solver

- [Backends](backends/index.md)

  - LLVM
  - Michelson
  - Plonk

----------

```{toctree}
---
maxdepth: 1
---

frontend/index
core/index
backends/index
```

----------