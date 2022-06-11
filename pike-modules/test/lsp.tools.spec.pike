import Pest;
import LSP;

int main() {
describe("Helper tools", lambda () {
  test(
    "deep_find_property() should find properties in different depths",
    lambda () {
      mapping m = ([
        "name": "Pike",
        "type": ([
          "areas": ({ "network", "server", "all purpose" }),
          "syntax": "C-like",
          "creator": ([
            "firstname": "Fredrik",
            "lastname": "Hübinette",
          ])
        ])
      ]);

      string name = Tools.deep_find_property(m, "name");
      expect(name)->to_equal("Pike");
      string syntax = Tools.deep_find_property(m, "syntax");
      expect(syntax)->to_equal("C-like");
      string lastname = Tools.deep_find_property(m, "lastname");
      expect(lastname)->to_equal("Hübinette");
    }
  );
});
}
