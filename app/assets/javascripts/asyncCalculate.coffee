
# Wire up to the button on document load
$ ->
  submit = $('#submit')
  expressionField = $('#expression')

  submit.click (event) ->
    event.preventDefault()
    expression = expressionField.val()

    $.post '/calculate.json'
      expression: expression
      (data) ->
        # Check for result box
        result = $('.result')

        if result.length is 0
          $('h1').after '<div class="result" style="display: none"></div>'
          result = $('.result')

        result.html '<div class="expression">Question: ' + data.expression + '</div><div class="answer">Answer: ' + data.result + '</div>'
        result.fadeIn()
