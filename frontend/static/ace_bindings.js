function ace_edit(dom_id) {
  var editor = ace.edit(dom_id);
  return editor
}

function ace_set_theme(editor, theme) {
  editor.setTheme(theme);
  return editor
}

function ace_set_font_size(editor, size) {
  editor.setFontSize(size);
  return editor
}

function ace_get_session(editor) {
  return editor.session
}

function ace_get_renderer(editor) {
  return editor.renderer
}

function ace_set_mode(editor, mode) {
  editor.session.setMode(mode)
}

function ace_set_keyboard_handler(editor, handler) {
  editor.setKeyboardHandler(handler)
}

function ace_test() {
  console.log("ace_test");
}

function ace_get_document(editor) {
  return editor.session.getDocument();
}

function ace_get_all_lines(editor) {
  return editor.session.getDocument().getAllLines();
}

function ace_get_line(editor, row) {
  return editor.session.getDocument().getLine(row);
}


function ace_add_callback(editor, fn) {
  editor.session.on('change', fn);
}

function ace_get_length(editor) {
  return editor.session.getDocument().getLength();
}

function ace_set_show_gutter(editor, show_status) {
  editor.renderer.setShowGutter(show_status);
}


function ace_manual_init() {
  var editor = ace.edit("editor");
  editor.setTheme("ace/theme/monokai");
  editor.session.setMode("ace/mode/markdown");
  editor.renderer.setShowGutter(false);
  editor.setFontSize("20px");
  editor.setKeyboardHandler("ace/keyboard/vim");
  return editor
}
