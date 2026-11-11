# The shelf's API in Rails's API mode (ActionController::API), for
# comparing with servant. Called through Rack, so no server is started.
# 本棚の API を Rails の API モード（ActionController::API）で。servant と
# くらべるため。Rack を通して呼ぶので、サーバーは立てない
require "action_controller"
require "json"
require "rack/mock"

RATINGS = { 1 => 90 }

# ANCHOR: controller
# Ruby (Rails's API mode: a controller with no views, JSON only)
# Ruby（Rails の API モード：ビューのないコントローラー。JSON だけ）
class RatingsController < ActionController::API
  def update
    id = Integer(params[:id])
    points = params[:points] # nil when missing / なければ nil
    unless points.is_a?(Integer) && points.between?(0, 100)
      return refuse(:unprocessable_content, "bad-score", "点は 0〜100 です")
    end
    unless RATINGS.key?(id)
      return refuse(:not_found, "no-such-work", "本棚にない作品です")
    end

    RATINGS[id] = points
    head :no_content
  end

  private

  def refuse(status, mistake, message)
    render json: { mistake: mistake, messages: [message] }, status: status
  end
end
# ANCHOR_END: controller

# ANCHOR: routes
# Ruby (the routes, as config/routes.rb would draw them)
# Ruby（道。config/routes.rb で描くのと同じ）
ROUTES = ActionDispatch::Routing::RouteSet.new
ROUTES.draw do
  # only digits for :id; "abc" never reaches the controller (404)
  # :id は数字だけ。「abc」はコントローラーに届かない（404）
  put "/works/:id/rating", to: "ratings#update", constraints: { id: /\d+/ }
end
# ANCHOR_END: routes

def put_rating(id, body)
  Rack::MockRequest.new(ROUTES).put(
    "/works/#{id}/rating",
    "CONTENT_TYPE" => "application/json", input: JSON.generate(body)
  )
end

res = put_rating(1, { points: 70 })
raise "204: #{res.status}" unless res.status == 204
raise "70" unless RATINGS[1] == 70

res = put_rating(1, { points: 101 })
raise "422: #{res.status}" unless res.status == 422
raise "bad-score" unless JSON.parse(res.body)["mistake"] == "bad-score"
raise "still 70" unless RATINGS[1] == 70

res = put_rating(99, { points: 50 })
raise "404: #{res.status}" unless res.status == 404

res = put_rating(1, {})
raise "no points: #{res.status}" unless res.status == 422
raise "no points" unless JSON.parse(res.body)["mistake"] == "bad-score"

res = put_rating("abc", { points: 50 })
raise "abc: #{res.status}" unless res.status == 404

# Only what is needed for an API: no cookies, no views.
# API に要るものだけ：クッキーもビューもない
api = ActionController::API.ancestors
raise "no cookies" if api.include?(ActionController::Cookies)
puts "ok"
