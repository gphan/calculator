
# Wire up to the button on document load
$ ->
  submit = $('#submit')
  expressionField = $('#expression')

  submit.click (event) ->
    event.preventDefault()
    expression = expressionField.val()

    # Check for result box
    result = $('.result')

    if result.length is 0
      $('h1').after '<div class="result" style="display: none"></div>'
      result = $('.result')

    result.append('<div class="expression">&gt; ' + expression + '</div>').fadeIn()

    $.post(
      '/calculate.json'
      expression: expression
      (data) ->
        result.append('<div class="answer">&rarr; ' + data.result + '</div>').fadeIn()
    )
