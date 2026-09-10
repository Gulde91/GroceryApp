// Holder den aktive indkøbsseddel i den aktuelle browsers localStorage.
(function () {
  'use strict';

  var STORAGE_KEY = 'groceryapp.cart.draft';
  var MAX_STORED_CHARACTERS = 2 * 1024 * 1024;
  var handlersRegistered = false;
  var hasLoaded = false;
  var lastKnownValue = null;

  function sendInput(inputId, value) {
    if (!inputId || !window.Shiny || !window.Shiny.setInputValue) return;
    Shiny.setInputValue(inputId, value, { priority: 'event' });
  }

  function unavailable(inputId, requestId) {
    sendInput(inputId, {
      status: 'unavailable',
      request_id: requestId,
      nonce: Date.now()
    });
  }

  function registerHandlers() {
    if (
      handlersRegistered ||
      !window.Shiny ||
      !window.Shiny.addCustomMessageHandler
    ) {
      return handlersRegistered;
    }

    Shiny.addCustomMessageHandler(
      'groceryapp_cart_draft_load',
      function (message) {
        if (!message || typeof message.input_id !== 'string') return;

        try {
          var raw = window.localStorage.getItem(STORAGE_KEY);
          hasLoaded = true;
          lastKnownValue = raw;
          if (raw === null) {
            sendInput(message.input_id, {
              status: 'empty',
              request_id: message.request_id
            });
          } else if (raw.length > MAX_STORED_CHARACTERS) {
            sendInput(message.input_id, {
              status: 'invalid',
              request_id: message.request_id
            });
          } else {
            sendInput(message.input_id, {
              status: 'found',
              request_id: message.request_id,
              value: raw
            });
          }
        } catch (error) {
          unavailable(message.input_id, message.request_id);
        }
      }
    );

    Shiny.addCustomMessageHandler(
      'groceryapp_cart_draft_save',
      function (message) {
        if (!message) return;

        try {
          if (!hasLoaded) {
            unavailable(message.status_input_id, message.request_id);
            return;
          }
          var currentValue = window.localStorage.getItem(STORAGE_KEY);
          if (currentValue !== lastKnownValue) {
            sendInput(message.status_input_id, {
              status: 'conflict',
              request_id: message.request_id,
              nonce: Date.now()
            });
            return;
          }
          if (message.clear === true) {
            window.localStorage.removeItem(STORAGE_KEY);
            lastKnownValue = null;
            return;
          }
          if (typeof message.value !== 'string') return;
          window.localStorage.setItem(STORAGE_KEY, message.value);
          lastKnownValue = message.value;
        } catch (error) {
          unavailable(message.status_input_id, message.request_id);
        }
      }
    );

    handlersRegistered = true;
    return true;
  }

  if (!registerHandlers()) {
    if (window.jQuery) {
      window.jQuery(document).on(
        'shiny:connected.cartPersistence',
        registerHandlers
      );
    } else {
      document.addEventListener('DOMContentLoaded', registerHandlers, {
        once: true
      });
    }
  }
})();
