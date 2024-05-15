import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

public class WebCrawler {

    private static final int MAX_DEPTH = 5;
    private Set<String> links;

    public WebCrawler() {
        links = new HashSet<>();
    }

    public void getPageLinks(String URL, int depth) {
        if ((!links.contains(URL) && (depth < MAX_DEPTH))) {
            System.out.println("Depth: " + depth + " [" + URL + "]");
            try {
                links.add(URL);
                Document document = Jsoup.connect(URL).get();
                Elements linksOnPage = document.select("a[href]");
                depth++;
                for (Element page : linksOnPage) {
                    getPageLinks(page.attr("abs:href"), depth);
                }
            } catch (IOException e) {
                System.err.println("For '" + URL + "': " + e.getMessage());
            }
        }
    }

    public static void main(String[] args) {
        WebCrawler webCrawler = new WebCrawler();
        webCrawler.getPageLinks("https://example.com", 0);
    }
}

