package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableAuditedImpl;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.script.ScriptExecutor;
import org.cyk.utility.__kernel__.script.ScriptHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ScopeFunction.TABLE_NAME)
@AttributeOverrides(value= {
		@AttributeOverride(name = ScopeFunction.FIELD___AUDIT_WHO__,column = @Column(name="AUDIT_ACTEUR"))
		,@AttributeOverride(name = ScopeFunction.FIELD___AUDIT_WHAT__,column = @Column(name="AUDIT_ACTION"))
		,@AttributeOverride(name = ScopeFunction.FIELD___AUDIT_WHEN__,column = @Column(name="AUDIT_DATE"))
		,@AttributeOverride(name = ScopeFunction.FIELD___AUDIT_FUNCTIONALITY__,column = @Column(name="AUDIT_FONCTIONALITE"))
})
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_WRITE)
public class ScopeFunction extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableAuditedImpl implements MeaEntity,Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_SCOPE) @NotNull private Scope scope;
	@Transient private String scopeAsString;
	@Transient private String scopeIdentifier;
	@Transient private String scopeCode;
	@Transient private ScopeTypeFunction scopeTypeFunction;
	
	@ManyToOne @JoinColumn(name = COLUMN_FUNCTION) @NotNull private Function function;
	@Transient private String functionCode;
	@Transient private String functionAsString;
	@Transient private Boolean isHolder;
	
	@ManyToOne @JoinColumn(name = COLUMN_LOCALITY) private Locality locality;
	@Transient private String localityAsString;
	@Transient private String localityIdentifier;
	@Transient private String localityCode;
	
	//@ManyToOne @JoinColumn(name = COLUMN_ACTIVITY) private Activity activity;
	@Column(name = COLUMN_ACTIVITY_IDENTIFIER) private String activityIdentifier;
	//@Transient private String activityAsString;
	//@Transient private String activityIdentifier;
	//@Transient private String activityCode;
	
	@Column(name = COLUMN_NUMBER_OF_ACTOR) private Integer numberOfActor;
	@Column(name = COLUMN_ACTORS_CODES) private String actorsCodes;
	@Transient private Collection<String> actorsNames;
	
	@Transient private Collection<String> actorsAsStrings;
	@Transient private String actorAsString;
	@Column(name = COLUMN_DOCUMENT_NUMBER) private Integer documentNumber;
	@Column(name = COLUMN_ORDER_NUMBER) private Integer orderNumber;
	
	@Column(name = COLUMN_PARENT_IDENTIFIER) private String parentIdentifier;
	@Transient private ScopeFunction parent;
	@Transient private String parentCode;
	@Transient private String parentAsString;
	@Transient private Byte childrenCount;
	@Transient private Collection<String> childrenIdentifiers;
	@Transient private Collection<String> childrenCodesNames;
	
	@Column(name = COLUMN_CODIFICATION_DATE) private LocalDateTime codificationDate;
	
	@Column(name = "ETAT") private String __mea_statut__;
	@Column(name = "DATE_ETAT") private LocalDateTime __mea_date_statut__;
	
	@Transient private Boolean shared;
	@Transient private String sharedAsString;
	@Transient private BudgetSpecializationUnit budgetSpecializationUnit;
	
	@Transient private Boolean requested;
	@Transient private String requestedAsString;
	@Transient private Boolean granted;
	@Transient private String grantedAsString;
		
	@Transient private String assignmentToActorMessage;
	
	public Byte incrementChildrenCount() {		
		if(childrenCount == null)
			childrenCount = 1;
		else
			childrenCount++;
		return childrenCount;
	}
	
	@Override
	public MeaEntity writeStatus() {
		__mea_statut__ = "MODI";
		__mea_date_statut__ = LocalDateTime.now();
		return this;
	}
	
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
	
	public static void computeCodeAndName(String scopeTypeCode,Collection<ScopeFunction> scopeFunctions,Integer orderNumber1,Integer orderNumber2,String codeScript,String nameScript) {
		if(CollectionHelper.isEmpty(scopeFunctions))
			return;
		String script = formatScript(scopeTypeCode,orderNumber1,orderNumber2,codeScript, nameScript);
		Collection<ScriptDto> dtos = ScriptDto.collect(scopeFunctions);
		LogHelper.logInfo(String.format("Codification du code et du libelle de %s poste(s) en cours...", scopeFunctions.size()), ScopeFunction.class);
		Long t = System.currentTimeMillis();
		ScriptExecutor.getInstance().execute(script, "l",dtos);
		LogHelper.logInfo(String.format("Codification du code et du libelle de %s poste(s) éffectué(s) en %s", scopeFunctions.size()
				,TimeHelper.formatDuration(System.currentTimeMillis()-t)), ScopeFunction.class);
	}
	
	public static String formatScript(String scopeTypeCode,Integer orderNumber1,Integer orderNumber2,String codeScript,String nameScript) { 
		return StringUtils.join(
				ScriptHelper.formatFunction("generateCode", ScriptHelper.formatReturnIfNotExist(codeScript), "poste")
				,ScriptHelper.formatFunction("generateName", ScriptHelper.formatReturnIfNotExist(nameScript), "poste")
				)
				+String.format("var numero_ordre = %s;",orderNumber1)
				+String.format("var numero_ordre_2 = %s;",orderNumber2)
				+ScriptHelper.join(						
						"var numero_ordre_1 = 0;"
						,"var numero_ordre_3 = 0;"
			,ScriptHelper.formatLoopForCollection("l", "i", ScriptHelper.join(
					"l.get(i).scopeFunction.code = generateCode(l.get(i))" //"l.get(i).code = generateCode("+formatGeneratorFunctionArguments("code")+")"
					,"l.get(i).scopeFunction.name = generateName(l.get(i))" //"l.get(i).name = generateName("+formatGeneratorFunctionArguments("name")+")"
					)
				)
		);
	}
	
	public static String formatGeneratorFunctionArguments(String fieldName) {
		return String.format(List.of(FIELD_SCOPE,FIELD_FUNCTION,FIELD_LOCALITY).stream().map(x -> "l.get(i)."+x+".%1$s").collect(Collectors.joining(",")), fieldName);
	}
	
	@Override
	public String toString() {
		return code+" "+name;
	}
	
	public static final String FIELD_SCOPE = "scope";
	public static final String FIELD_SCOPE_AS_STRING = "scopeAsString";
	public static final String FIELD_SCOPE_IDENTIFIER = "scopeIdentifier";
	public static final String FIELD_SCOPE_CODE = "scopeCode";
	
	public static final String FIELD_FUNCTION = "function";
	public static final String FIELD_FUNCTION_CODE = "functionCode";
	public static final String FIELD_FUNCTION_AS_STRING = "functionAsString";
	public static final String FIELD_IS_HOLDER = "isHolder";
	
	public static final String FIELD_LOCALITY = "locality";
	public static final String FIELD_LOCALITY_AS_STRING = "localityAsString";
	public static final String FIELD_LOCALITY_IDENTIFIER = "localityIdentifier";
	public static final String FIELD_LOCALITY_CODE = "localityCode";
	
	public static final String FIELD_ACTIVITY_IDENTIFIER = "activityIdentifier";
	
	public static final String FIELD_NUMBER_OF_ACTOR = "numberOfActor";
	public static final String FIELD_ACTORS_CODES = "actorsCodes";
	public static final String FIELD_ACTORS_NAMES = "actorsNames";
	public static final String FIELD_ACTORS_AS_STRINGS = "actorsAsStrings";
	public static final String FIELD_SHARED = "shared";
	public static final String FIELD_SHARED_AS_STRING = "sharedAsString";
	public static final String FIELD_PARENT_IDENTIFIER = "parentIdentifier";
	public static final String FIELD_DOCUMENT_NUMBER = "documentNumber";
	public static final String FIELD_ORDER_NUMBER = "orderNumber";
	public static final String FIELD_CODIFICATION_DATE = "codificationDate";
	
	public static final String FIELD_REQUESTED = "requested";
	public static final String FIELD_REQUESTED_AS_STRING = "requestedAsString";
	public static final String FIELD_GRANTED = "granted";
	public static final String FIELD_GRANTED_AS_STRING = "grantedAsString";
	
	public static final String FIELD_ACTOR_AS_STRING = "actorAsString";
	public static final String FIELD_ASSIGNMENT_TO_ACTOR_MESSAGE = "assignmentToActorMessage";
	
	public static final String TABLE_NAME = "POSTE";
	
	public static final String COLUMN_SCOPE = "DOMAINE";
	public static final String COLUMN_FUNCTION = "FONCTION";
	public static final String COLUMN_LOCALITY = "LOCALITE";
	public static final String COLUMN_NUMBER_OF_ACTOR = "NOMBRE_ACTEUR";
	public static final String COLUMN_ACTORS_CODES = "NOMS_UTILISATEURS";
	public static final String COLUMN_PARENT_IDENTIFIER = "PARENT";
	public static final String COLUMN_ACTIVITY_IDENTIFIER = "ACTIVITE";
	public static final String COLUMN_DOCUMENT_NUMBER = "NUMERO_DOCUMENT";
	public static final String COLUMN_ORDER_NUMBER = "NUMERO_ORDRE";
	public static final String COLUMN_CODIFICATION_DATE = "DATE_CODIFICATION";
	
	public static final String CODE_SCRIPT_VARIABLE_NAME_SCOPE_CODE_FORMAT = "code_%s";
	public static final String CODE_SCRIPT_VARIABLE_NAME_SCOPE_NAME_FORMAT = "libelle_%s";
	
	public static final String CODE_SCRIPT_VARIABLE_NAME_FUNCTION_CODE = "code_fonction";
	public static final String CODE_SCRIPT_VARIABLE_NAME_FUNCTION_NAME = "libelle_fonction";
	
	/**/
	
	public static class ScriptDto {
		public ScopeFunction scopeFunction;
		public Holder titulaire;
		
		/* Scopes */
		public CodeName section = new CodeName();
		public CodeName usb = new CodeName();
		public CodeName ua = new CodeName();
		public CodeName service_ord = new CodeName();
		public CodeName service_cf = new CodeName();
		public CodeName service_cpt = new CodeName();
		
		/* Functions */
		public CodeName fonction = new CodeName();
		
		/* Localities */
		public CodeName localite = new CodeName();
		
		/* Activities */
		public CodeName activite = new CodeName();
		
		/**/
		
		@Override
		public String toString() {
			return ToStringBuilder.reflectionToString(this, ToStringStyle.DEFAULT_STYLE);
		}
		
		public static ScriptDto map(ScopeFunction scopeFunction) {
			if(scopeFunction == null)
				return null;
			ScriptDto scriptDto = new ScriptDto();
			scriptDto.scopeFunction = scopeFunction;
			if(scopeFunction.parent != null) {
				scriptDto.titulaire = new Holder();
				scriptDto.titulaire.scopeFunction = scopeFunction.parent;
				scriptDto.titulaire.code = scopeFunction.parent.code;
				scriptDto.titulaire.libelle = scopeFunction.parent.name;
				scriptDto.titulaire.nombreAssistant = scopeFunction.parent.childrenCount;
			}
			/* Scopes */
			CodeName scope = null;
			if(scopeFunction.getScope().getType().getCode().equals(ScopeType.CODE_SECTION))
				scope = scriptDto.section;				
			else if(scopeFunction.getScope().getType().getCode().equals(ScopeType.CODE_USB))
				scope = scriptDto.usb;	
			else if(scopeFunction.getScope().getType().getCode().equals(ScopeType.CODE_UA))
				scope = scriptDto.ua;
			else if(scopeFunction.getScope().getType().getCode().equals(ScopeType.CODE_SERVICE_ORD))
				scope = scriptDto.service_ord;
			else if(scopeFunction.getScope().getType().getCode().equals(ScopeType.CODE_SERVICE_CF))
				scope = scriptDto.service_cf;
			else if(scopeFunction.getScope().getType().getCode().equals(ScopeType.CODE_SERVICE_CPT))
				scope = scriptDto.service_cpt;
			if(scope != null) {
				scope.code = scopeFunction.getScope().getCode();
				scope.libelle = scopeFunction.getScope().getName();
			}
			
			if(scopeFunction.budgetSpecializationUnit != null) {
				scriptDto.usb.code = scopeFunction.budgetSpecializationUnit.getCode();
				scriptDto.usb.libelle = scopeFunction.budgetSpecializationUnit.getName();
				
				if(scopeFunction.budgetSpecializationUnit.getSection() != null) {
					scriptDto.section.code = scopeFunction.budgetSpecializationUnit.getSection().getCode();
					scriptDto.section.libelle = scopeFunction.budgetSpecializationUnit.getSection().getName();
				}
			}
			
			if(StringHelper.isBlank(scopeFunction.activityIdentifier)) {
				scriptDto.activite = null;
			}else {
				scriptDto.activite.code = scopeFunction.activityIdentifier;
				scriptDto.activite.libelle = scopeFunction.activityIdentifier;
			}
			
			/* Functions */
			scriptDto.fonction.code = scopeFunction.function.getCode();
			scriptDto.fonction.libelle = scopeFunction.function.getName();
			
			/* Localities */
			if(scopeFunction.locality == null) {
				scriptDto.localite = null;
			}else {
				scriptDto.localite.code = scopeFunction.locality.getCode();
				scriptDto.localite.libelle = scopeFunction.locality.getName();
			}
			
			/* Activities */
			if(StringHelper.isBlank(scopeFunction.activityIdentifier)) {
				scriptDto.activite = null;
			}else {
				scriptDto.activite.code = scopeFunction.activityIdentifier;
				scriptDto.activite.libelle = scopeFunction.activityIdentifier;
			}
			return scriptDto;
		}
		
		public static Collection<ScriptDto> collect(Collection<ScopeFunction> scopeFunctions) {
			if(CollectionHelper.isEmpty(scopeFunctions))
				return null;
			return scopeFunctions.stream().map(scopeFunction -> map(scopeFunction)).collect(Collectors.toList());
		}
		
		/**/
		
		public static class CodeName {
			public String code;
			public String libelle;
			
			/*@Override
			public String toString() {
				return code+" "+libelle;
			}*/
		}
		
		public static class Holder extends CodeName {
			public ScopeFunction scopeFunction;
			public Byte nombreAssistant;
			
			public Byte incrementerNombreAssistant() {
				if(scopeFunction == null) {
					if(nombreAssistant == null)
						nombreAssistant = 1;
					else
						nombreAssistant++;
				}else {
					nombreAssistant = scopeFunction.incrementChildrenCount();
				}			
				return nombreAssistant;
			}
		}
	}
}