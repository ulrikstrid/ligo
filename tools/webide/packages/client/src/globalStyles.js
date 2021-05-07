import { createGlobalStyle } from 'styled-components';

export const GlobalStyles = createGlobalStyle`
  *,
  *::after,
  *::before {
    box-sizing: border-box;
  }

:root {
    /* Note: the LIGO header should be ripped from the main ligolang.org homepage. Specs not included here :-) */
    /* width of all colored bands: 4px */
    --orange: #fc683a;
    --orange_trans: #fedace;
  
    --blue: rgba(14, 116, 255, 1);
    --button_float: rgba(14, 116, 255, 0.85);
    --blue_trans1: rgba(14, 116, 255, 0.15); /* #e1f1ff; */
    --blue_opaque1: #dbeaff;
    --blue_trans2: rgba(14, 116, 255, 0.08); /* #eff7ff; */
  
    --input_background: #eff7ff;
  
    --grey: #888;
  
    --box-shadow: 1px 3px 10px 0px rgba(153, 153, 153, 0.4); /* or #999999 */
    --border_radius: 3px;
  
    /* text, where 1rem = 16px */
    --font: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', 'Oxygen',
      'Ubuntu', 'Cantarell', 'Fira Sans', 'Droid Sans', 'Helvetica Neue',
      sans-serif;
    --font_weight: 400;
  
    --font_menu_hover: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto',
      'Oxygen', 'Ubuntu', 'Cantarell', 'Fira Sans', 'Droid Sans', 'Helvetica Neue',
      sans-serif;
    --font_menu_size: 1rem;
    --font_menu_color: rgba(0, 0, 0, 1);
  
    --font_sub_size: 0.8em;
    --font_sub_color: rgba(51, 51, 51, 1); /* or #333333; */
  
    --font_label: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto',
      'Oxygen', 'Ubuntu', 'Cantarell', 'Fira Sans', 'Droid Sans', 'Helvetica Neue',
      sans-serif;
    --font_label_size: 0.8rem;
    --font_label_color: rgba(153, 153, 153, 1); /* or #999999 */
  
    --font_code: Consolas, source-code-pro, Menlo, Monaco, 'Courier New',
      monospace;
    --font_code_size: 0.8rem;
    --font_code_color: rgba(51, 51, 51, 1); /* or #333333; */
  
    /* filler text for empty panel */
    --font_ghost: 2rem;
    --font_ghost_weight: 700;
    --font_ghost_color: rgba(153, 153, 153, 0.5); /* or #CFCFCF */
  
    --content_height:79vh;
  
    --tooltip_foreground: white;
    --tooltip_background: rgba(0, 0, 0, 0.75) /*#404040*/;
    --label_foreground: rgba(153, 153, 153, 1);
  }
  
  body {
    margin: 0;
    /* font-size: 1.1vw; */
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', 'Oxygen',
      'Ubuntu', 'Cantarell', 'Fira Sans', 'Droid Sans', 'Helvetica Neue',
      sans-serif;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    background: ${({ theme }) => theme.body};
    color: ${({ theme }) => theme.text};
  }
  
  code {
    font-family: source-code-pro, Menlo, Monaco, Consolas, 'Courier New',
      monospace;
  }

  #configList{
    background: ${({ theme }) => theme.body};
    color: ${({ theme }) => theme.text};
  }

  #command-select{
    background: ${({ theme }) => theme.body};
    color: ${({ theme }) => theme.text};
  }

  .margin-view-overlays{
    color: ${({ theme }) => theme.lightGrey};
  }

  .monaco-editor{
    color: ${({ theme }) => theme.lightGrey};
  }

  .headerLink{
    background: ${({ theme }) => theme.body};
    color: ${({ theme }) => theme.text};
  }
  
  .monaco-editor .current-line ~ .line-numbers {
    color: var(--orange);
    border-left: 4px solid var(--blue);
  }
  
  .monaco-editor .margin-view-overlays .current-line,
  .monaco-editor .view-overlays .current-line {
    background-color: var(--blue_trans1);
    color: var(--blue);
  }
  
  .Resizer {
    z-index: 3;
    -moz-box-sizing: border-box;
    -webkit-box-sizing: border-box;
    box-sizing: border-box;
    -moz-background-clip: padding;
    -webkit-background-clip: padding;
    background-clip: padding-box;
  }
    
  .Resizer:hover {
    -webkit-transition: all 2s ease;
    transition: all 2s ease;
  }
    
  .Resizer.horizontal {
    margin: -5px 0;
    border-top: 5px solid rgba(255, 255, 255, 0);
    cursor: row-resize;
  }
    
  .Resizer.horizontal:hover {
    border-top: 5px solid rgba(0, 0, 0, 0.311);
  }
  
  .Pane1 {
    min-height: 10vh;
    max-height: 99vh;
    z-index:0;
  }
  
  .Pane2 {
    z-index:2;
    overflow: auto;
  }  

  `;
