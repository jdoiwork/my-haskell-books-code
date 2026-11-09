# Read the sample EPUB with gepub, and walk its nav with Nokogiri.
# 見本の EPUB を gepub で読み、nav を Nokogiri でたどる
require "gepub"
require "nokogiri"

BOOKS = File.join(__dir__, "books")

# ANCHOR: gepub
# Ruby (with gepub and Nokogiri)
# Ruby（gepub と Nokogiri で）
def epub_toc(path)
  book = GEPUB::Book.parse(File.open(path))
  nav  = book.items.values.find do |item|
    item.properties.to_a.include?("nav")
  end
  doc  = Nokogiri::XML(nav.content)
  doc.remove_namespaces!
  walk = lambda do |ol, depth|
    ol.xpath("./li").flat_map do |li|
      [[depth, li.at_xpath("./a|./span").text.strip]] +
        li.xpath("./ol").flat_map { |child| walk.(child, depth + 1) }
    end
  end
  [book.title.to_s, walk.(doc.at_xpath("//nav[@type='toc']/ol"), 0)]
end
# ANCHOR_END: gepub

title, toc = epub_toc(File.join(BOOKS, "dragon.epub"))
raise title unless title == "はじめてのドラゴン学"
expected = [
  [0, "第1部 ドラゴンを知る"], [1, "第1章 見分け方"],
  [2, "1.1 うろこの色"], [2, "1.2 火を吐くもの"],
  [1, "第2章 すみかと季節"], [0, "第2部 ドラゴンと話す"],
  [1, "第3章 あいさつの作法"], [1, "第4章 怒らせたときは"]
]
raise toc.inspect unless toc == expected

# ANCHOR: broken
# Ruby: what gepub does with broken books
# Ruby：壊れた本を、gepub はどうするか
begin
  GEPUB::Book.parse(File.open(File.join(BOOKS, "hello.epub")))
rescue Zip::Error => e
  puts "hello.epub: #{e.message}"
end
nochap = GEPUB::Book.parse(File.open(File.join(BOOKS, "nochap.epub")))
missing = nochap.spine_items.select { |item| item.content.nil? }
puts "nochap.epub: #{missing.map(&:href).inspect}"
# ANCHOR_END: broken
raise missing.inspect unless missing.map(&:href) == ["ch2.xhtml"]
puts "gepub_toc.rb: ok"
