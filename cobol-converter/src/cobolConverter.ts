// COBOLプログラムの解析とJava変換のための型定義
export interface CobolProgram {
  programName: string;
  author?: string;
  dateWritten?: string;
  divisions: {
    identification?: CobolIdentificationDivision;
    environment?: CobolEnvironmentDivision;
    data?: CobolDataDivision;
    procedure?: CobolProcedureDivision;
  };
}

export interface CobolIdentificationDivision {
  programId: string;
  author?: string;
  dateWritten?: string;
  security?: string;
  remarks?: string[];
}

export interface CobolEnvironmentDivision {
  inputOutputSection?: {
    fileControl?: CobolFileControl[];
  };
}

export interface CobolFileControl {
  selectName: string;
  assignTo?: string;
  organization?: string;
  accessMode?: string;
  status?: string;
}

export interface CobolDataDivision {
  fileSection?: CobolFileSection[];
  workingStorageSection?: CobolWorkingStorageSection[];
}

export interface CobolFileSection {
  fdName: string;
  labelRecords?: string;
  recordingMode?: string;
  blockContains?: string;
  records: CobolRecord[];
}

export interface CobolWorkingStorageSection {
  level01Items: CobolLevel01Item[];
}

export interface CobolRecord {
  level: number;
  name: string;
  picture?: string;
  value?: string;
  occurs?: number;
  children?: CobolRecord[];
}

export interface CobolLevel01Item {
  name: string;
  picture?: string;
  value?: string;
  occurs?: number;
  children?: CobolRecord[];
}

export interface CobolProcedureDivision {
  paragraphs: CobolParagraph[];
}

export interface CobolParagraph {
  name: string;
  statements: CobolStatement[];
}

export interface CobolStatement {
  type: string;
  content: string;
  lineNumber?: number;
}

export interface ConversionResult {
  success: boolean;
  javaCode: string;
  errors: string[];
  warnings: string[];
}

// COBOLパーサークラス
export class CobolParser {
  parse(cobolCode: string): CobolProgram {
    const lines = cobolCode.split('\n');
    const program: CobolProgram = {
      programName: '',
      divisions: {
        identification: { programId: '' },
        environment: { inputOutputSection: { fileControl: [] } },
        data: { fileSection: [], workingStorageSection: [] },
        procedure: { paragraphs: [] }
      }
    };

    let currentDivision = '';
    let currentSection = '';
    let currentParagraph: CobolParagraph | null = null;
    let currentFileSection: CobolFileSection | null = null;
    let currentWorkingStorage: CobolWorkingStorageSection | null = null;
    let remarks: string[] = [];

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const trimmedLine = line.trim();
      
      // 空行をスキップ
      if (!trimmedLine) continue;

      // コメント行の処理
      if (trimmedLine.startsWith('*') || trimmedLine.startsWith('*>')) {
        const comment = trimmedLine.replace(/^\*[>]?\s*/, '');
        if (currentDivision === 'identification') {
          remarks.push(comment);
        }
        continue;
      }

      // DIVISIONの検出
      if (trimmedLine.includes('DIVISION')) {
        if (trimmedLine.includes('IDENTIFICATION')) {
          currentDivision = 'identification';
          program.divisions.identification = { programId: '', remarks: [] };
        } else if (trimmedLine.includes('ENVIRONMENT')) {
          currentDivision = 'environment';
          program.divisions.environment = { inputOutputSection: { fileControl: [] } };
        } else if (trimmedLine.includes('DATA')) {
          currentDivision = 'data';
          program.divisions.data = { fileSection: [], workingStorageSection: [] };
        } else if (trimmedLine.includes('PROCEDURE')) {
          currentDivision = 'procedure';
          program.divisions.procedure = { paragraphs: [] };
        }
        continue;
      }

      // SECTIONの検出
      if (trimmedLine.includes('SECTION')) {
        if (trimmedLine.includes('INPUT-OUTPUT')) {
          currentSection = 'input-output';
        } else if (trimmedLine.includes('FILE')) {
          currentSection = 'file';
        } else if (trimmedLine.includes('WORKING-STORAGE')) {
          currentSection = 'working-storage';
          currentWorkingStorage = { level01Items: [] };
          program.divisions.data?.workingStorageSection?.push(currentWorkingStorage);
        }
        continue;
      }

      // IDENTIFICATION DIVISIONの解析
      if (currentDivision === 'identification') {
        this.parseIdentificationLine(trimmedLine, program.divisions.identification!);
        continue;
      }

      // ENVIRONMENT DIVISIONの解析
      if (currentDivision === 'environment' && currentSection === 'input-output') {
        this.parseFileControl(trimmedLine, program.divisions.environment!);
        continue;
      }

      // DATA DIVISIONの解析
      if (currentDivision === 'data') {
        if (currentSection === 'file') {
          this.parseFileSection(trimmedLine, program.divisions.data!, currentFileSection);
        } else if (currentSection === 'working-storage') {
          this.parseWorkingStorage(trimmedLine, currentWorkingStorage!);
        }
        continue;
      }

      // PROCEDURE DIVISIONの解析
      if (currentDivision === 'procedure') {
        this.parseProcedureLine(trimmedLine, program.divisions.procedure!, currentParagraph);
      }
    }

    // プログラム名を設定
    if (program.divisions.identification?.programId) {
      program.programName = program.divisions.identification.programId;
    }

    return program;
  }

  private parseIdentificationLine(line: string, identification: CobolIdentificationDivision) {
    if (line.includes('PROGRAM-ID')) {
      const match = line.match(/PROGRAM-ID\.\s+(\w+)/i);
      if (match) {
        identification.programId = match[1];
      }
    } else if (line.includes('AUTHOR')) {
      const match = line.match(/AUTHOR\.\s+(.+)/i);
      if (match) {
        identification.author = match[1];
      }
    } else if (line.includes('DATE-WRITTEN')) {
      const match = line.match(/DATE-WRITTEN\.\s+(.+)/i);
      if (match) {
        identification.dateWritten = match[1];
      }
    } else if (line.includes('SECURITY')) {
      const match = line.match(/SECURITY\.\s+(.+)/i);
      if (match) {
        identification.security = match[1];
      }
    }
  }

  private parseFileControl(line: string, environment: CobolEnvironmentDivision) {
    if (line.includes('SELECT')) {
      const match = line.match(/SELECT\s+(\w+)/i);
      if (match) {
        const fileControl: CobolFileControl = {
          selectName: match[1]
        };
        
        // ASSIGN TO句の解析
        const assignMatch = line.match(/ASSIGN\s+TO\s+['"]([^'"]*)['"]/i);
        if (assignMatch) {
          fileControl.assignTo = assignMatch[1];
        }
        
        // ORGANIZATION句の解析
        const orgMatch = line.match(/ORGANIZATION\s+IS\s+(\w+)/i);
        if (orgMatch) {
          fileControl.organization = orgMatch[1];
        }
        
        // ACCESS MODE句の解析
        const accessMatch = line.match(/ACCESS\s+MODE\s+IS\s+(\w+)/i);
        if (accessMatch) {
          fileControl.accessMode = accessMatch[1];
        }
        
        // STATUS句の解析
        const statusMatch = line.match(/STATUS\s+IS\s+(\w+)/i);
        if (statusMatch) {
          fileControl.status = statusMatch[1];
        }
        
        environment.inputOutputSection?.fileControl?.push(fileControl);
      }
    }
  }

  private parseFileSection(line: string, data: CobolDataDivision, _currentFileSection: CobolFileSection | null) {
    // FD句の解析
    if (line.match(/^\s*FD\s+\w+/i)) {
      const match = line.match(/FD\s+(\w+)/i);
      if (match) {
        const fileSection: CobolFileSection = {
          fdName: match[1],
          records: []
        };
        data.fileSection?.push(fileSection);
        return fileSection;
      }
    }
    
    // レコード定義の解析
    if (line.match(/^\s*\d+\s+\w+/)) {
      const record = this.parseRecord(line);
      if (record && data.fileSection && data.fileSection.length > 0) {
        data.fileSection[data.fileSection.length - 1].records.push(record);
      }
    }
  }

  private parseWorkingStorage(line: string, workingStorage: CobolWorkingStorageSection) {
    if (line.match(/^\s*01\s+\w+/)) {
      const level01Item = this.parseLevel01Item(line);
      if (level01Item) {
        workingStorage.level01Items.push(level01Item);
      }
    }
  }

  private parseProcedureLine(line: string, procedure: CobolProcedureDivision, _currentParagraph: CobolParagraph | null) {
    // 段落名の解析
    if (line.match(/^\s*\d+\s+\w+\./)) {
      const match = line.match(/^\s*\d+\s+(\w+)\./);
      if (match) {
        const paragraph: CobolParagraph = {
          name: match[1],
          statements: []
        };
        procedure.paragraphs.push(paragraph);
        return paragraph;
      }
    }
    
    // 文の解析
    if (line.trim() && !line.match(/^\s*\d+\s+\w+\./)) {
      const statement: CobolStatement = {
        type: this.determineStatementType(line),
        content: line.trim()
      };
      
      if (procedure.paragraphs.length > 0) {
        procedure.paragraphs[procedure.paragraphs.length - 1].statements.push(statement);
      }
    }
  }

  private parseRecord(line: string): CobolRecord | null {
    const match = line.match(/^\s*(\d+)\s+(\w+)\s+(.+)/);
    if (!match) return null;

    const level = parseInt(match[1]);
    const name = match[2];
    const definition = match[3];

    const record: CobolRecord = {
      level,
      name
    };

    // PICTURE句の解析
    const pictureMatch = definition.match(/PIC\s+([X9A-Z(),.V]+)/i);
    if (pictureMatch) {
      record.picture = pictureMatch[1];
    }

    // VALUE句の解析
    const valueMatch = definition.match(/VALUE\s+([^.]*)/i);
    if (valueMatch) {
      record.value = valueMatch[1].trim();
    }

    // OCCURS句の解析
    const occursMatch = definition.match(/OCCURS\s+(\d+)/i);
    if (occursMatch) {
      record.occurs = parseInt(occursMatch[1]);
    }

    return record;
  }

  private parseLevel01Item(line: string): CobolLevel01Item | null {
    const match = line.match(/^\s*01\s+(\w+)\s+(.+)/);
    if (!match) return null;

    const name = match[1];
    const definition = match[2];

    const item: CobolLevel01Item = {
      name
    };

    // PICTURE句の解析
    const pictureMatch = definition.match(/PIC\s+([X9A-Z(),.V]+)/i);
    if (pictureMatch) {
      item.picture = pictureMatch[1];
    }

    // VALUE句の解析
    const valueMatch = definition.match(/VALUE\s+([^.]*)/i);
    if (valueMatch) {
      item.value = valueMatch[1].trim();
    }

    // OCCURS句の解析
    const occursMatch = definition.match(/OCCURS\s+(\d+)/i);
    if (occursMatch) {
      item.occurs = parseInt(occursMatch[1]);
    }

    return item;
  }

  private determineStatementType(line: string): string {
    const trimmed = line.trim().toUpperCase();
    
    if (trimmed.startsWith('DISPLAY')) return 'DISPLAY';
    if (trimmed.startsWith('MOVE')) return 'MOVE';
    if (trimmed.startsWith('IF')) return 'IF';
    if (trimmed.startsWith('END-IF')) return 'END-IF';
    if (trimmed.startsWith('PERFORM')) return 'PERFORM';
    if (trimmed.startsWith('READ')) return 'READ';
    if (trimmed.startsWith('WRITE')) return 'WRITE';
    if (trimmed.startsWith('OPEN')) return 'OPEN';
    if (trimmed.startsWith('CLOSE')) return 'CLOSE';
    if (trimmed.startsWith('COMPUTE')) return 'COMPUTE';
    if (trimmed.startsWith('ADD')) return 'ADD';
    if (trimmed.startsWith('SUBTRACT')) return 'SUBTRACT';
    if (trimmed.startsWith('MULTIPLY')) return 'MULTIPLY';
    if (trimmed.startsWith('DIVIDE')) return 'DIVIDE';
    if (trimmed.startsWith('ACCEPT')) return 'ACCEPT';
    if (trimmed.startsWith('STOP')) return 'STOP';
    if (trimmed.startsWith('EXIT')) return 'EXIT';
    
    return 'UNKNOWN';
  }
}

// Javaコンバータークラス
export class JavaConverter {
  convert(cobolProgram: CobolProgram): ConversionResult {
    const errors: string[] = [];
    const warnings: string[] = [];
    
    try {
      const javaCode = this.generateJavaCode(cobolProgram);
      return {
        success: true,
        javaCode,
        errors,
        warnings
      };
    } catch (error) {
      errors.push(`変換エラー: ${error instanceof Error ? error.message : 'Unknown error'}`);
      return {
        success: false,
        javaCode: '',
        errors,
        warnings
      };
    }
  }

  private generateJavaCode(program: CobolProgram): string {
    const className = this.toPascalCase(program.programName || 'CobolProgram');
    
    let javaCode = `import java.util.*;
import java.io.*;
import java.text.*;
import java.math.*;

/**
 * COBOLプログラム "${program.programName}" から変換されたJavaクラス
 * 生成日時: ${new Date().toLocaleString('ja-JP')}
${program.divisions.identification?.author ? ` * 作者: ${program.divisions.identification.author}` : ''}
${program.divisions.identification?.dateWritten ? ` * 作成日: ${program.divisions.identification.dateWritten}` : ''}
${program.divisions.identification?.security ? ` * セキュリティ: ${program.divisions.identification.security}` : ''}
 */
public class ${className} {
`;

    // ファイル制御変数の宣言
    if (program.divisions.environment?.inputOutputSection?.fileControl) {
      javaCode += `    // ファイル制御変数\n`;
      for (const fileControl of program.divisions.environment.inputOutputSection.fileControl) {
        const javaName = this.toCamelCase(fileControl.selectName);
        javaCode += `    private BufferedReader ${javaName}Reader;\n`;
        javaCode += `    private BufferedWriter ${javaName}Writer;\n`;
        if (fileControl.status) {
          javaCode += `    private String ${this.toCamelCase(fileControl.status)};\n`;
        }
      }
      javaCode += `\n`;
    }

    // ファイルセクションの変数宣言
    if (program.divisions.data?.fileSection) {
      javaCode += `    // ファイルレコード変数\n`;
      for (const fileSection of program.divisions.data.fileSection) {
        javaCode += `    // ${fileSection.fdName} レコード\n`;
        for (const record of fileSection.records) {
          if (record.level === 1) {
            const javaType = this.mapCobolTypeToJava(record.picture);
            const javaName = this.toCamelCase(record.name);
            javaCode += `    private ${javaType} ${javaName}`;
            
            if (record.value) {
              if (javaType === 'String') {
                javaCode += ` = "${record.value.replace(/['"]/g, '')}"`;
              } else if (javaType === 'int') {
                javaCode += ` = ${record.value}`;
              } else if (javaType === 'double') {
                javaCode += ` = ${record.value}`;
              }
            }
            
            javaCode += `;\n`;
          }
        }
        javaCode += `\n`;
      }
    }

    // ワーキングストレージセクションの変数宣言
    if (program.divisions.data?.workingStorageSection) {
      javaCode += `    // ワーキングストレージ変数\n`;
      for (const workingStorage of program.divisions.data.workingStorageSection) {
        for (const item of workingStorage.level01Items) {
          const javaType = this.mapCobolTypeToJava(item.picture);
          const javaName = this.toCamelCase(item.name);
          
          if (item.occurs) {
            javaCode += `    private ${javaType}[] ${javaName} = new ${javaType}[${item.occurs}];\n`;
          } else {
            javaCode += `    private ${javaType} ${javaName}`;
            
            if (item.value) {
              if (javaType === 'String') {
                javaCode += ` = "${item.value.replace(/['"]/g, '')}"`;
              } else if (javaType === 'int') {
                javaCode += ` = ${item.value}`;
              } else if (javaType === 'double') {
                javaCode += ` = ${item.value}`;
              }
            }
            
            javaCode += `;\n`;
          }
        }
      }
      javaCode += `\n`;
    }

    // コンストラクタ
    javaCode += `    public ${className}() {
        initializeVariables();
    }

    private void initializeVariables() {
        // 変数の初期化
`;

    // 変数の初期化
    if (program.divisions.data?.workingStorageSection) {
      for (const workingStorage of program.divisions.data.workingStorageSection) {
        for (const item of workingStorage.level01Items) {
          const javaName = this.toCamelCase(item.name);
          if (item.picture?.includes('X') || item.picture?.includes('A')) {
            javaCode += `        ${javaName} = "";\n`;
          } else if (item.picture?.includes('9')) {
            javaCode += `        ${javaName} = 0;\n`;
          }
        }
      }
    }

    javaCode += `    }

    public static void main(String[] args) {
        ${className} program = new ${className}();
        try {
            program.execute();
        } catch (Exception e) {
            System.err.println("プログラム実行エラー: " + e.getMessage());
            e.printStackTrace();
        }
    }

    public void execute() throws Exception {
`;

    // ファイルのオープン処理
    if (program.divisions.environment?.inputOutputSection?.fileControl) {
      javaCode += `        // ファイルのオープン\n`;
      for (const fileControl of program.divisions.environment.inputOutputSection.fileControl) {
        const javaName = this.toCamelCase(fileControl.selectName);
        if (fileControl.assignTo) {
          javaCode += `        try {\n`;
          javaCode += `            ${javaName}Reader = new BufferedReader(new FileReader("${fileControl.assignTo}"));\n`;
          javaCode += `        } catch (IOException e) {\n`;
          javaCode += `            System.err.println("ファイルオープンエラー: ${fileControl.assignTo}");\n`;
          javaCode += `        }\n`;
        }
      }
      javaCode += `\n`;
    }

    // 手続きの実行
    if (program.divisions.procedure?.paragraphs) {
      for (const paragraph of program.divisions.procedure.paragraphs) {
        javaCode += `        ${this.toCamelCase(paragraph.name)}();\n`;
      }
    }

    // ファイルのクローズ処理
    if (program.divisions.environment?.inputOutputSection?.fileControl) {
      javaCode += `\n        // ファイルのクローズ\n`;
      for (const fileControl of program.divisions.environment.inputOutputSection.fileControl) {
        const javaName = this.toCamelCase(fileControl.selectName);
        javaCode += `        if (${javaName}Reader != null) {\n`;
        javaCode += `            try { ${javaName}Reader.close(); } catch (IOException e) {}\n`;
        javaCode += `        }\n`;
      }
    }

    javaCode += `    }
`;

    // 各段落のメソッド化
    if (program.divisions.procedure?.paragraphs) {
      for (const paragraph of program.divisions.procedure.paragraphs) {
        javaCode += `
    private void ${this.toCamelCase(paragraph.name)}() {
`;
        
        for (const statement of paragraph.statements) {
          const javaStatement = this.convertStatement(statement);
          if (javaStatement) {
            javaCode += `        ${javaStatement}\n`;
          }
        }
        
        javaCode += `    }
`;
      }
    }

    // ユーティリティメソッド
    javaCode += `
    // ユーティリティメソッド
    private String formatNumber(double value, String picture) {
        if (picture == null) return String.valueOf(value);
        
        if (picture.includes("V")) {
            DecimalFormat df = new DecimalFormat("#.##");
            return df.format(value);
        } else if (picture.includes("9")) {
            return String.valueOf((int)value);
        }
        
        return String.valueOf(value);
    }
    
    private String padString(String value, int length) {
        if (value == null) value = "";
        if (value.length() > length) {
            return value.substring(0, length);
        }
        return String.format("%-" + length + "s", value);
    }
    
    private String padNumber(String value, int length) {
        if (value == null) value = "0";
        if (value.length() > length) {
            return value.substring(0, length);
        }
        return String.format("%" + length + "s", value).replace(' ', '0');
    }
`;

    javaCode += `}
`;

    return javaCode;
  }

  private mapCobolTypeToJava(picture?: string): string {
    if (!picture) return 'String';
    
    if (picture.includes('9')) {
      if (picture.includes('V') || picture.includes('.')) {
        return 'double';
      } else if (picture.includes('S')) {
        return 'int';
      } else {
        return 'int';
      }
    }
    if (picture.includes('X') || picture.includes('A')) return 'String';
    
    return 'String';
  }

  private convertStatement(statement: CobolStatement): string {
    const trimmed = statement.content.trim();
    
    switch (statement.type) {
      case 'DISPLAY':
        return this.convertDisplay(trimmed);
      case 'MOVE':
        return this.convertMove(trimmed);
      case 'IF':
        return this.convertIf(trimmed);
      case 'END-IF':
        return '}';
      case 'PERFORM':
        return this.convertPerform(trimmed);
      case 'READ':
        return this.convertRead(trimmed);
      case 'WRITE':
        return this.convertWrite(trimmed);
      case 'OPEN':
        return this.convertOpen(trimmed);
      case 'CLOSE':
        return this.convertClose(trimmed);
      case 'COMPUTE':
        return this.convertCompute(trimmed);
      case 'ADD':
        return this.convertAdd(trimmed);
      case 'SUBTRACT':
        return this.convertSubtract(trimmed);
      case 'MULTIPLY':
        return this.convertMultiply(trimmed);
      case 'DIVIDE':
        return this.convertDivide(trimmed);
      case 'ACCEPT':
        return this.convertAccept(trimmed);
      case 'STOP':
        return 'return;';
      case 'EXIT':
        return 'return;';
      default:
        return `// ${trimmed}`;
    }
  }

  private convertDisplay(statement: string): string {
    const content = statement.replace(/^DISPLAY\s+/i, '').replace(/\.$/, '');
    return `System.out.println(${content});`;
  }

  private convertMove(statement: string): string {
    const match = statement.match(/MOVE\s+(.+?)\s+TO\s+(\w+)/i);
    if (match) {
      const source = match[1];
      const target = this.toCamelCase(match[2]);
      return `${target} = ${source};`;
    }
    return `// ${statement}`;
  }

  private convertIf(statement: string): string {
    const condition = statement.replace(/^IF\s+/i, '').replace(/\s+THEN.*$/, '');
    const javaCondition = this.convertCondition(condition);
    return `if (${javaCondition}) {`;
  }

  private convertPerform(statement: string): string {
    const match = statement.match(/PERFORM\s+(\w+)/i);
    if (match) {
      return `${this.toCamelCase(match[1])}();`;
    }
    return `// ${statement}`;
  }

  private convertRead(statement: string): string {
    const match = statement.match(/READ\s+(\w+)/i);
    if (match) {
      const fileName = this.toCamelCase(match[1]);
      return `String line = ${fileName}Reader.readLine();`;
    }
    return `// ${statement}`;
  }

  private convertWrite(statement: string): string {
    const match = statement.match(/WRITE\s+(\w+)/i);
    if (match) {
      const fileName = this.toCamelCase(match[1]);
      return `${fileName}Writer.write(line);`;
    }
    return `// ${statement}`;
  }

  private convertOpen(statement: string): string {
    return `// ${statement}`;
  }

  private convertClose(statement: string): string {
    return `// ${statement}`;
  }

  private convertCompute(statement: string): string {
    const match = statement.match(/COMPUTE\s+(\w+)\s*=\s*(.+)/i);
    if (match) {
      const target = this.toCamelCase(match[1]);
      const expression = this.convertExpression(match[2]);
      return `${target} = ${expression};`;
    }
    return `// ${statement}`;
  }

  private convertAdd(statement: string): string {
    const match = statement.match(/ADD\s+(.+?)\s+TO\s+(\w+)/i);
    if (match) {
      const source = match[1];
      const target = this.toCamelCase(match[2]);
      return `${target} += ${source};`;
    }
    return `// ${statement}`;
  }

  private convertSubtract(statement: string): string {
    const match = statement.match(/SUBTRACT\s+(.+?)\s+FROM\s+(\w+)/i);
    if (match) {
      const source = match[1];
      const target = this.toCamelCase(match[2]);
      return `${target} -= ${source};`;
    }
    return `// ${statement}`;
  }

  private convertMultiply(statement: string): string {
    const match = statement.match(/MULTIPLY\s+(.+?)\s+BY\s+(\w+)/i);
    if (match) {
      const source = match[1];
      const target = this.toCamelCase(match[2]);
      return `${target} *= ${source};`;
    }
    return `// ${statement}`;
  }

  private convertDivide(statement: string): string {
    const match = statement.match(/DIVIDE\s+(.+?)\s+INTO\s+(\w+)/i);
    if (match) {
      const source = match[1];
      const target = this.toCamelCase(match[2]);
      return `${target} /= ${source};`;
    }
    return `// ${statement}`;
  }

  private convertAccept(statement: string): string {
    const match = statement.match(/ACCEPT\s+(\w+)/i);
    if (match) {
      const target = this.toCamelCase(match[1]);
      return `${target} = new Scanner(System.in).nextLine();`;
    }
    return `// ${statement}`;
  }

  private convertCondition(condition: string): string {
    return condition
      .replace(/=/g, '==')
      .replace(/NOT\s+/gi, '!')
      .replace(/AND/gi, '&&')
      .replace(/OR/gi, '||')
      .replace(/GREATER\s+THAN/gi, '>')
      .replace(/LESS\s+THAN/gi, '<')
      .replace(/EQUAL\s+TO/gi, '==');
  }

  private convertExpression(expression: string): string {
    return expression
      .replace(/\*\*/g, 'Math.pow')
      .replace(/\*\*/g, ',')
      .replace(/\*\*/g, ')');
  }

  private toPascalCase(str: string): string {
    return str.charAt(0).toUpperCase() + str.slice(1).toLowerCase();
  }

  private toCamelCase(str: string): string {
    return str.charAt(0).toLowerCase() + str.slice(1).toLowerCase();
  }
}
