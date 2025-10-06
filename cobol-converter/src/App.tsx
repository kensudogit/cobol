import React, { useState, useRef, useEffect } from 'react';
import { CobolParser, JavaConverter, ConversionResult } from './cobolConverter';

function App() {
  const [cobolCode, setCobolCode] = useState('');
  const [javaCode, setJavaCode] = useState('');
  const [isConverting, setIsConverting] = useState(false);
  const [conversionResult, setConversionResult] = useState<ConversionResult | null>(null);
  const [isDarkMode, setIsDarkMode] = useState(false);
  const fileInputRef = useRef<HTMLInputElement>(null);

  // ダークモード切り替え
  useEffect(() => {
    document.documentElement.setAttribute('data-theme', isDarkMode ? 'dark' : 'light');
  }, [isDarkMode]);

  const toggleDarkMode = () => {
    setIsDarkMode(!isDarkMode);
  };

  const handleFileUpload = (event: React.ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0];
    if (file) {
      const reader = new FileReader();
      reader.onload = (e) => {
        const content = e.target?.result as string;
        setCobolCode(content);
        setJavaCode('');
        setConversionResult(null);
      };
      reader.readAsText(file);
    }
  };

  const handleDragOver = (event: React.DragEvent) => {
    event.preventDefault();
  };

  const handleDrop = (event: React.DragEvent) => {
    event.preventDefault();
    const file = event.dataTransfer.files[0];
    if (file && file.name.endsWith('.cob')) {
      const reader = new FileReader();
      reader.onload = (e) => {
        const content = e.target?.result as string;
        setCobolCode(content);
        setJavaCode('');
        setConversionResult(null);
      };
      reader.readAsText(file);
    }
  };

  const handleConvert = async () => {
    if (!cobolCode.trim()) {
      alert('COBOLコードを入力してください。');
      return;
    }

    setIsConverting(true);
    setConversionResult(null);

    try {
      // COBOLパーサーでコードを解析
      const parser = new CobolParser();
      const cobolProgram = parser.parse(cobolCode);

      // Javaコンバーターで変換
      const converter = new JavaConverter();
      const result = converter.convert(cobolProgram);

      setConversionResult(result);
      if (result.success) {
        setJavaCode(result.javaCode);
      }
    } catch (error) {
      setConversionResult({
        success: false,
        javaCode: '',
        errors: [`変換中にエラーが発生しました: ${error instanceof Error ? error.message : 'Unknown error'}`],
        warnings: []
      });
    } finally {
      setIsConverting(false);
    }
  };

  const handleDownloadJava = () => {
    if (!javaCode) return;

    const blob = new Blob([javaCode], { type: 'text/java' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'ConvertedProgram.java';
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  };

  return (
    <div className="container">
      {/* ダークモードトグルボタン */}
      <button className="theme-toggle" onClick={toggleDarkMode}>
        {isDarkMode ? '☀️' : '🌙'}
      </button>

      <div className="header">
        <h1>COBOL to Java Converter</h1>
        <p>COBOLプログラムをJavaプログラムに変換します</p>
      </div>

      <div className="converter-section">
        <div className="file-input-section">
          <h3>COBOLファイルの選択</h3>
          <div
            className={`file-input ${cobolCode ? 'has-content' : ''}`}
            onDragOver={handleDragOver}
            onDrop={handleDrop}
            onClick={() => fileInputRef.current?.click()}
          >
            {cobolCode ? (
              <div>
                <p>✅ COBOLファイルが読み込まれました</p>
                <p>クリックして別のファイルを選択</p>
              </div>
            ) : (
              <div>
                <p>📁 COBOLファイル（.cob）をドラッグ&ドロップ</p>
                <p>またはクリックしてファイルを選択</p>
              </div>
            )}
          </div>
          <input
            ref={fileInputRef}
            type="file"
            accept=".cob,.cbl"
            onChange={handleFileUpload}
            style={{ display: 'none' }}
          />
        </div>

        <div className="code-section">
          <div className="code-block">
            <h3>COBOLコード</h3>
            <textarea
              value={cobolCode}
              onChange={(e) => setCobolCode(e.target.value)}
              placeholder="COBOLコードを入力するか、ファイルをアップロードしてください..."
              style={{
                width: '100%',
                height: '300px',
                fontFamily: 'Courier New, monospace',
                fontSize: '14px',
                padding: '10px',
                border: '1px solid #ddd',
                borderRadius: '4px',
                resize: 'vertical'
              }}
            />
          </div>

          <div className="code-block">
            <h3>変換されたJavaコード</h3>
            <div className="code-content">
              {javaCode || '変換ボタンをクリックしてJavaコードを生成してください'}
            </div>
            {javaCode && (
              <button
                className="download-button"
                onClick={handleDownloadJava}
              >
                📥 Javaファイルをダウンロード
              </button>
            )}
          </div>
        </div>

        <div style={{ textAlign: 'center', marginTop: '20px' }}>
          <button
            className="convert-button"
            onClick={handleConvert}
            disabled={isConverting || !cobolCode.trim()}
          >
            {isConverting ? (
              <>
                <span className="loading"></span>
                変換中...
              </>
            ) : (
              '🔄 変換実行'
            )}
          </button>
        </div>

        {conversionResult && (
          <div className={`status ${conversionResult.success ? 'success' : 'error'}`}>
            {conversionResult.success ? (
              <div>
                <h4>✅ 変換完了</h4>
                <p>COBOLプログラムが正常にJavaプログラムに変換されました。</p>
                {conversionResult.warnings.length > 0 && (
                  <div>
                    <h5>⚠️ 警告:</h5>
                    <ul>
                      {conversionResult.warnings.map((warning, index) => (
                        <li key={index}>{warning}</li>
                      ))}
                    </ul>
                  </div>
                )}
              </div>
            ) : (
              <div>
                <h4>❌ 変換エラー</h4>
                <ul>
                  {conversionResult.errors.map((error, index) => (
                    <li key={index}>{error}</li>
                  ))}
                </ul>
              </div>
            )}
          </div>
        )}
      </div>

      <div className="usage-section">
        <h3>使用方法</h3>
        <ol>
          <li><strong>COBOLファイルの選択:</strong> .cobまたは.cblファイルをドラッグ&ドロップするか、クリックして選択してください</li>
          <li><strong>コードの確認:</strong> 左側のテキストエリアでCOBOLコードを確認・編集できます</li>
          <li><strong>変換実行:</strong> 「変換実行」ボタンをクリックしてJavaコードを生成します</li>
          <li><strong>結果の確認:</strong> 右側に生成されたJavaコードが表示されます</li>
          <li><strong>ダウンロード:</strong> 「Javaファイルをダウンロード」ボタンで.javaファイルを保存できます</li>
        </ol>

        <h3>🔧 対応機能</h3>
        <ul>
          <li>IDENTIFICATION DIVISION（プログラム名の抽出）</li>
          <li>DATA DIVISION（変数定義の変換）</li>
          <li>PROCEDURE DIVISION（処理ロジックの変換）</li>
          <li>DISPLAY文（System.out.printlnへの変換）</li>
          <li>MOVE文（代入文への変換）</li>
          <li>IF文（条件分岐の変換）</li>
          <li>PERFORM文（メソッド呼び出しへの変換）</li>
        </ul>
      </div>
    </div>
  );
}

export default App;
