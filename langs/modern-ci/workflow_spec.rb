# Check the shape of the CI files in code/ci/, for the chapter
# "CI and reproducible environments". (We cannot run GitHub Actions
# here, so we read the files and check what they say.)
# code/ci/ の CI のファイルの形を確かめる。「CI と再現できる環境」の章
# （ここでは GitHub Actions を動かせないので、ファイルを読んで確かめる）
require "yaml"
require "json"

CI = File.expand_path("../../ci", __dir__)

RSpec.describe "the workflow" do
  workflow = YAML.load_file(File.join(CI, "haskell.yml"))
  build = workflow["jobs"]["build"]

  # 2つの OS と2つの GHC で、4通り
  it "builds on two OSes with two GHCs" do
    matrix = build["strategy"]["matrix"]
    expect(matrix["os"].size * matrix["ghc"].size).to eq(4)
  end

  # キャッシュを戻してから、ビルドとテストをする
  it "restores the cache before building and testing" do
    names = build["steps"].map { |s| s["name"] }.compact
    expect(names).to eq(["Plan the build", "Restore the dependencies",
                         "Build the dependencies", "Save the dependencies",
                         "Build", "Test"])
  end

  # hlint の仕事もある
  it "has an hlint job" do
    expect(workflow["jobs"].keys).to contain_exactly("build", "hlint")
  end
end

RSpec.describe "the other files" do
  # Dependabot は action を見る
  it "dependabot watches the actions" do
    config = YAML.load_file(File.join(CI, "dependabot.yml"))
    expect(config["updates"].first["package-ecosystem"]).to eq("github-actions")
  end

  # devcontainer は Dockerfile を使う
  it "the devcontainer uses the Dockerfile" do
    config = JSON.parse(File.read(File.join(CI, "devcontainer/devcontainer.json")))
    expect(config["build"]["dockerfile"]).to eq("Dockerfile")
  end
end
