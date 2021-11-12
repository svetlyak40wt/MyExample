package moc.stna04.example;

import android.os.Bundle;

import com.google.android.material.floatingactionbutton.FloatingActionButton;
import com.google.android.material.snackbar.Snackbar;

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

import android.util.Log;
import android.view.View;

import android.view.Menu;
import android.view.MenuItem;
import android.webkit.WebView;
import android.widget.TextView;

import java.util.concurrent.TimeUnit;

public class MainActivity extends AppCompatActivity {
    Integer initialized = -1;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        Toolbar toolbar = findViewById(R.id.toolbar);
        setSupportActionBar(toolbar);

        FloatingActionButton fab = findViewById(R.id.fab);
        fab.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                Snackbar.make(view, "интересно а русский?!", Snackbar.LENGTH_LONG)
                        .setAction("Action", null).show();
            }
        });

        tryInit();
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_main, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();

        //noinspection SimplifiableIfStatement
        if (id == R.id.action_settings) {
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    private void tryInit (){
        int lwStatus = com.lispworks.Manager.status ()  ;
        switch (lwStatus) {
            case com.lispworks.Manager.STATUS_READY : lispInitialized(); break;
            case com.lispworks.Manager.STATUS_ERROR : lispNotInitialized(); break ;
            case com.lispworks.Manager.STATUS_INITIALIZING :
            case  com.lispworks.Manager.STATUS_NOT_INITIALIZED :
                // Initialize with a runnable that will get called when LispWorks
                // finished to initialize (or got an error) and will run this method again
                // to update "initialized" attribute.
                Runnable rn = new Runnable () { public void run() { tryInit () ; } };
                com.lispworks.Manager.init(this, rn);
                break ;
        }
    }

    private void lispNotInitialized() {
        String msg = (
                "Failed to initialize LispWorks("
                        + com.lispworks.Manager.init_result_code() + ") : "
                        + com.lispworks.Manager.mInitErrorString
        );
        Log.d("app", msg);
        TextView errorText = (TextView) findViewById(R.id.error_text);
        errorText.setText(msg);
    }
    private void lispInitialized() {
        TextView errorText = (TextView) findViewById(R.id.error_text);
        errorText.setText("Lisp Was Initialized");

        Object url = com.lispworks.LispCalls.callObjectV("GET-URL-TO-DISPLAY");
        WebView myWebView = (WebView) findViewById(R.id.web_view);
        myWebView.loadUrl((String) url);
    }
}