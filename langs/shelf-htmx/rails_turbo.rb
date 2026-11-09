# Notes with Rails and Hotwire's Turbo Streams, for comparing with htmx.
# Called through Rack, so no server is started. Turbo itself (the JS)
# runs in the browser; here we check what the server answers.
# Rails と Hotwire の Turbo Streams で感想を足す。htmx とくらべるため。
# Rack を通して呼ぶので、サーバーは立てない。Turbo そのもの（JS）は
# ブラウザーで動く。ここでは、サーバーが何を答えるかを確かめる
require "action_controller"
require "action_view"
require "rack/mock"

NOTES = [{ body: "絵が多くて読みやすい" }]

# ANCHOR: controller
# Ruby (Rails: answer one note as a Turbo Stream, "prepend to #notes")
# Ruby（Rails：感想1つを Turbo Stream で答える。「#notes の先頭に足して」）
class NotesController < ActionController::Base
  # the view, as app/views/notes/create.turbo_stream.erb would hold it
  # ビュー。app/views/notes/create.turbo_stream.erb に置くのと同じ
  NOTE = <<~ERB
    <turbo-stream action="prepend" target="notes">
      <template><li><%= note[:body] %></li></template>
    </turbo-stream>
  ERB

  def create
    body = params.fetch(:body, "").strip
    return head(:unprocessable_content) if body.empty?

    note = { body: body }
    NOTES.unshift(note)
    render inline: NOTE, locals: { note: note },
           content_type: "text/vnd.turbo-stream.html"
  end
end
# ANCHOR_END: controller

ROUTES = ActionDispatch::Routing::RouteSet.new
ROUTES.draw do
  post "/works/:id/notes", to: "notes#create"
end

def post_note(body)
  Rack::MockRequest.new(ROUTES).post(
    "/works/1/notes",
    "CONTENT_TYPE" => "application/x-www-form-urlencoded",
    "HTTP_ACCEPT" => "text/vnd.turbo-stream.html",
    input: URI.encode_www_form(body: body)
  )
end

res = post_note("<script>alert(1)</script> 竜の章")
raise "200: #{res.status}" unless res.status == 200
raise "type" unless res.content_type.start_with?("text/vnd.turbo-stream.html")
raise "prepend" unless res.body.include?('<turbo-stream action="prepend" target="notes">')
# ERB escapes what is written, as lucid does
# ERB も、lucid と同じく、書いたものをエスケープする
raise "escaped: #{res.body}" unless res.body.include?("&lt;script&gt;alert(1)&lt;/script&gt; 竜の章")
raise "two notes" unless NOTES.size == 2

res = post_note("   ")
raise "422: #{res.status}" unless res.status == 422
raise "still two" unless NOTES.size == 2
puts "ok"
