package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.validation.constraints.NotNull;

import org.apache.commons.lang3.StringUtils;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.script.ScriptExecutor;
import org.cyk.utility.__kernel__.script.ScriptHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ScopeFunction.TABLE_NAME,uniqueConstraints = {
		@UniqueConstraint(name=ScopeFunction.TABLE_NAME+"_"+ScopeFunction.COLUMN_SCOPE+"_"+ScopeFunction.COLUMN_FUNCTION+"_UK"
				,columnNames = {ScopeFunction.COLUMN_SCOPE,ScopeFunction.COLUMN_FUNCTION})
})
public class ScopeFunction extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_SCOPE) @NotNull private Scope scope;
	@ManyToOne @JoinColumn(name = COLUMN_FUNCTION) @NotNull private Function function;
	@Column(name = COLUMN_NUMBER_OF_ACTOR) private Integer numberOfActor;
	
	@Transient private String scopeAsString;
	@Transient private String functionAsString;
	@Transient private Boolean shared;
	@Transient private String sharedAsString;
	@Transient private ScopeTypeFunction scopeTypeFunction;
	
	@Override
	public ScopeFunction setIdentifier(String identifier) {
		return (ScopeFunction) super.setIdentifier(identifier);
	}
	
	@Override
	public ScopeFunction setCode(String code) {
		return (ScopeFunction) super.setCode(code);
	}
	
	@Override
	public ScopeFunction setName(String name) {
		return (ScopeFunction) super.setName(name);
	}
	
	public ScopeFunction setScopeFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setScope(null);
		else
			setScope(EntityFinder.getInstance().find(Scope.class, identifier));
		return this;
	}
	
	public ScopeFunction setFunctionFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setFunction(null);
		else
			setFunction(EntityFinder.getInstance().find(Function.class, identifier));
		return this;
	}
	
	public ScopeFunction setCodeFromScript() {
		return setCodeFromScript(scopeTypeFunction.getScopeFunctionCodeScript());
	}

	public ScopeFunction setNameFromScript() {
		return setNameFromScript(scopeTypeFunction.getScopeFunctionNameScript());
	}
	
	public ScopeFunction computeAndSetCode(String script,String scopeTypeCode,String scopeCode,String functionCode) {
		code = computeCode(script, scopeTypeCode, scopeCode, functionCode);
		return this;
	}
	
	public ScopeFunction setCodeFromScript(String script) {
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("script", script);
		code = ScriptExecutor.getInstance().execute(String.class, script,getScriptVariables(this));
		return this;
	}
	
	public ScopeFunction computeAndSetName(String script,String scopeTypeCode,String scopeName,String functionName) {
		name = computeName(script, scopeTypeCode, scopeName, functionName);
		return this;
	}
	
	public ScopeFunction setNameFromScript(String script) {
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("script", script);
		name = ScriptExecutor.getInstance().execute(String.class, script,getScriptVariables(this));
		return this;
	}
	
	public static Object[] getScriptVariables(ScopeFunction scopeFunction) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("scopeFunction", scopeFunction);
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("scope", scopeFunction.scope);
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("scopeType", scopeFunction.scope.getType());
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("function", scopeFunction.function);
		return new Object[] {
				getScriptVariableNameScopeCode(scopeFunction.scope.getType().getCode()),scopeFunction.scope.getCode()
			,getScriptVariableNameScopeName(scopeFunction.scope.getType().getCode()),scopeFunction.scope.getName()
			,CODE_SCRIPT_VARIABLE_NAME_FUNCTION_CODE,scopeFunction.function.getCode()
			,CODE_SCRIPT_VARIABLE_NAME_FUNCTION_NAME,scopeFunction.function.getName()
		};
	}
	
	public static String[] getScriptVariablesNames(ScopeFunction scopeFunction) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("scopeFunction", scopeFunction);
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("scope", scopeFunction.scope);
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("scopeType", scopeFunction.scope.getType());
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("function", scopeFunction.function);
		return new String[] {getScriptVariableNameScopeCode(scopeFunction.scope.getType().getCode())
			,getScriptVariableNameScopeName(scopeFunction.scope.getType().getCode())
			,CODE_SCRIPT_VARIABLE_NAME_FUNCTION_CODE
			,CODE_SCRIPT_VARIABLE_NAME_FUNCTION_NAME
		};
	}
	
	public static String getScriptVariableNameScopeCode(String scopeTypeCode) {
		if(StringHelper.isBlank(scopeTypeCode))
			return null;
		return String.format(CODE_SCRIPT_VARIABLE_NAME_SCOPE_CODE_FORMAT, scopeTypeCode.toLowerCase());
	}
	
	public static String getScriptVariableNameScopeName(String scopeTypeCode) {
		if(StringHelper.isBlank(scopeTypeCode))
			return null;
		return String.format(CODE_SCRIPT_VARIABLE_NAME_SCOPE_NAME_FORMAT, scopeTypeCode.toLowerCase());
	}
	
	public static String computeCode(String script,String scopeTypeCode,String scopeCode,String functionCode) {
		if(StringHelper.isBlank(script))
			return functionCode+scopeCode;
		return ScriptExecutor.getInstance().execute(String.class, script, getScriptVariableNameScopeCode(scopeTypeCode),scopeCode
				,CODE_SCRIPT_VARIABLE_NAME_FUNCTION_CODE,functionCode);
	}
	
	public static String computeCode(String script,String scopeTypeCode,Scope scope,Function function) {
		return computeCode(script, scopeTypeCode, scope == null ? null : scope.getCode(), function == null ? null : function.getCode());
	}
	
	public static String computeName(String script,String scopeTypeCode,String scopeName,String functionName) {
		if(StringHelper.isBlank(script))
			return functionName+" "+scopeName;
		return ScriptExecutor.getInstance().execute(String.class, script, getScriptVariableNameScopeName(scopeTypeCode),scopeName
				,CODE_SCRIPT_VARIABLE_NAME_FUNCTION_NAME,functionName);
	}
	
	public static String computeName(String script,String scopeTypeCode,Scope scope,Function function) {
		return computeName(script, scopeTypeCode, scope == null ? null : scope.getName(), function == null ? null : function.getName());
	}
	
	public static Boolean isExistsByCodesOnly(Collection<ScopeFunction> scopeFunctions,Scope scope,Function function) {
		if(CollectionHelper.isEmpty(scopeFunctions) || scope == null || function == null)
			return null;
		for(ScopeFunction scopeFunction : scopeFunctions)
			if(scope.getCode().equals(scopeFunction.getScopeAsString()) && function.getCode().equals(scopeFunction.getFunctionAsString()))
				return Boolean.TRUE;
		return Boolean.FALSE;
	}
	
	public static ScopeFunction find(String scopeCode,String functionCode,Collection<ScopeFunction> scopeFunctions) {
		if(StringHelper.isBlank(scopeCode) || StringHelper.isBlank(functionCode) || CollectionHelper.isEmpty(scopeFunctions))
			return null;
		for(ScopeFunction scopeFunction : scopeFunctions)
			if(scopeFunction.getScope().getCode().equals(scopeCode) && scopeFunction.getFunction().getCode().equals(functionCode))
				return scopeFunction;
		return null;
	}
	
	public static void computeCodeAndName(String scopeTypeCode,Collection<ScopeFunction> scopeFunctions,String codeScript,String nameScript) {
		if(CollectionHelper.isEmpty(scopeFunctions))
			return;
		String script = formatScript(scopeTypeCode,codeScript, nameScript);
		ScriptExecutor.getInstance().execute(script, "l",scopeFunctions);		
	}
	
	public static String formatScript(String scopeTypeCode,String codeScript,String nameScript) { 
		return StringUtils.join(
				ScriptHelper.formatFunction("generateCode", ScriptHelper.formatReturnIfNotExist(codeScript), getScriptVariableNameScopeCode(scopeTypeCode),"code_fonction")
				,ScriptHelper.formatFunction("generateName", ScriptHelper.formatReturnIfNotExist(nameScript), getScriptVariableNameScopeName(scopeTypeCode),"libelle_fonction"))
				+ScriptHelper.join(
				
			ScriptHelper.formatLoopForCollection("l", "i", ScriptHelper.join(
					"l.get(i).code = generateCode(l.get(i).scope.code,l.get(i).function.code)"
					,"l.get(i).name = generateName(l.get(i).scope.name,l.get(i).function.name)"
					)
				)
		);
	}
	
	@Override
	public String toString() {
		return function+"-"+scope;
	}
	
	public static final String FIELD_SCOPE = "scope";
	public static final String FIELD_FUNCTION = "function";
	public static final String FIELD_SCOPE_AS_STRING = "scopeAsString";
	public static final String FIELD_FUNCTION_AS_STRING = "functionAsString";
	public static final String FIELD_NUMBER_OF_ACTOR = "numberOfActor";
	public static final String FIELD_SHARED = "shared";
	public static final String FIELD_SHARED_AS_STRING = "sharedAsString";
	
	public static final String TABLE_NAME = "POSTE";
	
	public static final String COLUMN_SCOPE = "domaine";
	public static final String COLUMN_FUNCTION = "fonction";
	public static final String COLUMN_NUMBER_OF_ACTOR = "nombre_acteur";
	
	public static final String CODE_SCRIPT_VARIABLE_NAME_SCOPE_CODE_FORMAT = "code_%s";
	public static final String CODE_SCRIPT_VARIABLE_NAME_SCOPE_NAME_FORMAT = "libelle_%s";
	
	public static final String CODE_SCRIPT_VARIABLE_NAME_FUNCTION_CODE = "code_fonction";
	public static final String CODE_SCRIPT_VARIABLE_NAME_FUNCTION_NAME = "libelle_fonction";
	
}