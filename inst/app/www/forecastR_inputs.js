Shiny.addCustomMessageHandler("updateLabel",
  function(message) {
    for (var i = 0; i < message.ids.length; i++) {
      element = $("[data-id=" + message.ids[i] + "]");
      for (var j = 0; j < element.length; j++) {
        element[j].innerHTML = message.values[i];
      }
    }
  }
);

var binding = new Shiny.InputBinding();

$.extend(binding, {
  find: function(scope) {
    return $(scope).find(".input-group-date");
  },

  getValue: function(el) {
    return $(el).find("input").val();
  },

  subscribe: function(el, callback) {
    $(el).on("dp.change", function(event) {
      callback();
    });
  },

  unsubscribe: function(el) {
    $(el).off(".binding");
  }
});

Shiny.inputBindings.register(binding);