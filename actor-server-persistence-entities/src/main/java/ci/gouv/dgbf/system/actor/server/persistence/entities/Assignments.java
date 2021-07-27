package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedStoredProcedureQueries;
import javax.persistence.NamedStoredProcedureQuery;
import javax.persistence.ParameterMode;
import javax.persistence.StoredProcedureParameter;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringAuditedImpl;
import org.cyk.utility.__kernel__.object.marker.IdentifiableSystem;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.hibernate.envers.AuditOverride;
import org.hibernate.envers.AuditOverrides;
import org.hibernate.envers.Audited;
import org.hibernate.envers.RelationTargetAuditMode;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Assignments.TABLE_NAME)
@AttributeOverrides(value= {
		@AttributeOverride(name = Assignments.FIELD___AUDIT_WHO__,column = @Column(name=Assignments.COLUMN___AUDIT_WHO__))
		,@AttributeOverride(name = Assignments.FIELD___AUDIT_WHAT__,column = @Column(name=Assignments.COLUMN___AUDIT_WHAT__))
		,@AttributeOverride(name = Assignments.FIELD___AUDIT_WHEN__,column = @Column(name=Assignments.COLUMN___AUDIT_WHEN__))
		,@AttributeOverride(name = Assignments.FIELD___AUDIT_FUNCTIONALITY__,column = @Column(name=Assignments.COLUMN___AUDIT_FUNCTIONALITY__))
})
@NamedStoredProcedureQueries(value = {
	@NamedStoredProcedureQuery(
			name = Assignments.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_CLEAN, 
			procedureName = Assignments.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_CLEAN
			,parameters = {
				@StoredProcedureParameter(name = Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTOR , mode = ParameterMode.IN,type = String.class)
				,@StoredProcedureParameter(name = Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_FUNCTIONALITY , mode = ParameterMode.IN,type = String.class)
				,@StoredProcedureParameter(name = Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTION , mode = ParameterMode.IN,type = String.class)
				,@StoredProcedureParameter(name = Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_DATE , mode = ParameterMode.IN,type = java.sql.Date.class)
			}
	),@NamedStoredProcedureQuery(
			name = Assignments.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_IMPORT, 
			procedureName = Assignments.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_IMPORT
			,parameters = {
					@StoredProcedureParameter(name = Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTOR , mode = ParameterMode.IN,type = String.class)
					,@StoredProcedureParameter(name = Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_FUNCTIONALITY , mode = ParameterMode.IN,type = String.class)
					,@StoredProcedureParameter(name = Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTION_CREATE , mode = ParameterMode.IN,type = String.class)
					,@StoredProcedureParameter(name = Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTION_UPDATE , mode = ParameterMode.IN,type = String.class)
					,@StoredProcedureParameter(name = Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_DATE , mode = ParameterMode.IN,type = java.sql.Date.class)
				}
	),@NamedStoredProcedureQuery(
			name = Assignments.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_IMPORT_NEWS, 
			procedureName = Assignments.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_IMPORT_NEWS
			,parameters = {
					@StoredProcedureParameter(name = Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTOR , mode = ParameterMode.IN,type = String.class)
					,@StoredProcedureParameter(name = Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_FUNCTIONALITY , mode = ParameterMode.IN,type = String.class)
					,@StoredProcedureParameter(name = Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTION , mode = ParameterMode.IN,type = String.class)
					,@StoredProcedureParameter(name = Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_DATE , mode = ParameterMode.IN,type = java.sql.Date.class)
				}
	),@NamedStoredProcedureQuery(
			name = Assignments.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_EXPORT, 
			procedureName = Assignments.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_EXPORT
			,parameters = {
					@StoredProcedureParameter(name = Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTOR , mode = ParameterMode.IN,type = String.class)
					,@StoredProcedureParameter(name = Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_FUNCTIONALITY , mode = ParameterMode.IN,type = String.class)
					,@StoredProcedureParameter(name = Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTION , mode = ParameterMode.IN,type = String.class)
					,@StoredProcedureParameter(name = Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_DATE , mode = ParameterMode.IN,type = java.sql.Date.class)
				}
		)
})
//@Audited
@AuditOverrides(value = {
		@AuditOverride(forClass = AbstractIdentifiableSystemScalarStringAuditedImpl.class)
})
public class Assignments extends AbstractIdentifiableSystemScalarStringAuditedImpl implements MeaEntity,Serializable {

	/* Imputation */
	//@Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)
	@ManyToOne @JoinColumn(name=COLUMN_EXECUTION_IMPUTATION) private ExecutionImputation executionImputation;
	
	@Transient private String sectionAsString;
	@Transient private String budgetSpecializationUnitAsString;
	@Transient private String actionAsString;
	@Transient private String activityAsString;
	@Transient private String economicNatureAsString;
	@Transient private String administrativeUnitAsString;
	@Transient private String activityCategoryAsString;
	@Transient private String expenditureNatureAsString;
	
	/* Affectations */
	
	/* Gestionnaire de crédits */
	
	@Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)
	@ManyToOne @JoinColumn(name=COLUMN_CREDIT_MANAGER_HOLDER) private ScopeFunction creditManagerHolder;
	@Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)
	@ManyToOne @JoinColumn(name=COLUMN_CREDIT_MANAGER_ASSISTANT) private ScopeFunction creditManagerAssistant;
	
	/* Ordonnateur */
	
	@Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)
	@ManyToOne @JoinColumn(name=COLUMN_AUTHORIZING_OFFICER_HOLDER) private ScopeFunction authorizingOfficerHolder;
	@Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)
	@ManyToOne @JoinColumn(name=COLUMN_AUTHORIZING_OFFICER_ASSISTANT) private ScopeFunction authorizingOfficerAssistant;
	
	/* Controleur financier */
	
	@Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)
	@ManyToOne @JoinColumn(name=COLUMN_FINANCIAL_CONTROLLER_HOLDER) private ScopeFunction financialControllerHolder;
	@Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)
	@ManyToOne @JoinColumn(name=COLUMN_FINANCIAL_CONTROLLER_ASSISTANT) private ScopeFunction financialControllerAssistant;
	
	/* Comptable */
	
	@Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)
	@ManyToOne @JoinColumn(name=COLUMN_ACCOUNTING_HOLDER) private ScopeFunction accountingHolder;
	@Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)
	@ManyToOne @JoinColumn(name=COLUMN_ACCOUNTING_ASSISTANT) private ScopeFunction accountingAssistant;
	
	/**/
	
	@Column(name = "ETAT") private String __mea_statut__;
	@Column(name = "DATE_ETAT") private LocalDateTime __mea_date_statut__;
	
	/**/
	
	@Transient private String creditManagerHolderAsString;
	@Transient private String creditManagerAssistantAsString;	
	@Transient private String authorizingOfficerHolderAsString;
	@Transient private String authorizingOfficerAssistantAsString;	
	@Transient private String financialControllerHolderAsString;
	@Transient private String financialControllerAssistantAsString;	
	@Transient private String accountingHolderAsString;
	@Transient private String accountingAssistantAsString;
	
	@Transient private Collection<Assignments> __auditRecords__;
	/**/
	
	public Assignments(String identifier,String executionImputationIdentifier
			,String creditManagerHolderIdentifier,String creditManagerAssistantIdentifier
			,String authorizingOfficerHolder,String authorizingOfficerAssistant
			,String financialControllerHolder,String financialControllerAssistant
			,String accountingHolder,String accountingAssistant
			) {
		this.identifier = identifier;
		this.executionImputation = new ExecutionImputation().setIdentifier(executionImputationIdentifier);
		this.creditManagerHolder = new ScopeFunction().setIdentifier(creditManagerHolderIdentifier);
		this.creditManagerAssistant = new ScopeFunction().setIdentifier(creditManagerAssistantIdentifier);
		this.authorizingOfficerHolder = new ScopeFunction().setIdentifier(authorizingOfficerHolder);
		this.authorizingOfficerAssistant = new ScopeFunction().setIdentifier(authorizingOfficerAssistant);
		this.financialControllerHolder = new ScopeFunction().setIdentifier(financialControllerHolder);
		this.financialControllerAssistant = new ScopeFunction().setIdentifier(financialControllerAssistant);
		this.accountingHolder = new ScopeFunction().setIdentifier(accountingHolder);
		this.accountingAssistant = new ScopeFunction().setIdentifier(accountingAssistant);
	}
	
	@Override
	public Assignments setIdentifier(String identifier) {
		return (Assignments) super.setIdentifier(identifier);
	}
	
	@Override
	public MeaEntity writeStatus() {
		__mea_statut__ = "MODI";
		__mea_date_statut__ = LocalDateTime.now();
		return this;
	}
	
	public static Boolean isOneHolderHasChanged(Assignments assignments1,Assignments assignments2,Collection<String> fieldsNames) {
		if(assignments1 == null || assignments2 == null || CollectionHelper.isEmpty(fieldsNames))
			return Boolean.FALSE;
		for(String fieldName : fieldsNames) {
			if(StringHelper.isBlank(fieldName))
				continue;
			if(isOneHolderHasChanged(assignments1, assignments2, fieldName))
				return Boolean.TRUE;
		}
		return Boolean.FALSE;
	}
	
	private static Boolean isOneHolderHasChanged(Assignments assignments1,Assignments assignments2,String fieldName) {
		if(!Boolean.TRUE.equals(IdentifiableSystem.areIdentifiersEqual((ScopeFunction)FieldHelper.read(assignments1, fieldName)
				, (ScopeFunction)FieldHelper.read(assignments2, fieldName))))
			return Boolean.TRUE;
		return Boolean.FALSE;
	}
	
	/**/
	
	public static final String FIELD_EXECUTION_IMPUTATION = "executionImputation";
	public static final String FIELD_SECTION_AS_STRING = "sectionAsString";
	public static final String FIELD_BUDGET_SPECIALIZATION_UNIT_AS_STRING = "budgetSpecializationUnitAsString";
	public static final String FIELD_ACTION_AS_STRING = "actionAsString";
	public static final String FIELD_ACTIVITY_AS_STRING = "activityAsString";
	public static final String FIELD_ECONOMIC_NATURE_AS_STRING = "economicNatureAsString";
	public static final String FIELD_ADMINISTRATIVE_UNIT_AS_STRING = "administrativeUnitAsString";
	public static final String FIELD_ACTIVITY_CATEGORY_AS_STRING = "activityCategoryAsString";
	public static final String FIELD_EXPENDITURE_NATURE_AS_STRING = "expenditureNatureAsString";
	public static final String FIELD_CREDIT_MANAGER_HOLDER = "creditManagerHolder";
	public static final String FIELD_CREDIT_MANAGER_ASSISTANT = "creditManagerAssistant";
	public static final String FIELD_AUTHORIZING_OFFICER_HOLDER = "authorizingOfficerHolder";
	public static final String FIELD_AUTHORIZING_OFFICER_ASSISTANT = "authorizingOfficerAssistant";
	public static final String FIELD_FINANCIAL_CONTROLLER_HOLDER = "financialControllerHolder";
	public static final String FIELD_FINANCIAL_CONTROLLER_ASSISTANT = "financialControllerAssistant";
	public static final String FIELD_ACCOUNTING_HOLDER = "accountingHolder";
	public static final String FIELD_ACCOUNTING_ASSISTANT = "accountingAssistant";
	
	public static final String FIELD_CREDIT_MANAGER_HOLDER_AS_STRING = "creditManagerHolderAsString";
	public static final String FIELD_CREDIT_MANAGER_ASSISTANT_AS_STRING = "creditManagerAssistantAsString";
	public static final String FIELD_AUTHORIZING_OFFICER_HOLDER_AS_STRING = "authorizingOfficerHolderAsString";
	public static final String FIELD_AUTHORIZING_OFFICER_ASSISTANT_AS_STRING = "authorizingOfficerAssistantAsString";
	public static final String FIELD_FINANCIAL_CONTROLLER_HOLDER_AS_STRING = "financialControllerHolderAsString";
	public static final String FIELD_FINANCIAL_CONTROLLER_ASSISTANT_AS_STRING = "financialControllerAssistantAsString";
	public static final String FIELD_ACCOUNTING_HOLDER_AS_STRING = "accountingHolderAsString";
	public static final String FIELD_ACCOUNTING_ASSISTANT_AS_STRING = "accountingAssistantAsString";
	
	//public static final String FIELD___ALL__ = "__all__";
	
	public static final String FIELDS_ALL = "all";
	public static final String FIELDS_ALL_STRINGS = "allStrings";
	public static final String FIELDS_ALL_STRINGS_CODES_ONLY = "allStringsCodesOnly";
	public static final String FIELDS_ALL_STRINGS_CODES_NAMES_WITH_ASSISTANTS = "allStringsCodesNamesWithAssistants";
	public static final String FIELDS_ALL_STRINGS_CODES_ONLY_WITHOUT_SCOPE_FUNCTIONS = "allStringsCodesOnlyWithoutScopeFunctions";
	public static final String FIELDS_ALL_STRINGS_NAMES_ONLY_OR_CODES_ONLY = "allStringsNamesOnlyOrCodesOnly";
	
	public static final String FIELDS_HOLDERS = "holders";
	
	public static final List<String> FIELDS_SCOPES_FUNCTIONS_HOLDERS = List.of(
			FIELD_CREDIT_MANAGER_HOLDER,FIELD_AUTHORIZING_OFFICER_HOLDER,FIELD_FINANCIAL_CONTROLLER_HOLDER,FIELD_ACCOUNTING_HOLDER
		);
	
	public static final List<String> FIELDS_SCOPE_FUNCTIONS_HOLDERS_AS_STRING = new ArrayList<>();
	static {
		FIELDS_SCOPES_FUNCTIONS_HOLDERS.forEach(fieldName -> {
			FIELDS_SCOPE_FUNCTIONS_HOLDERS_AS_STRING.add(fieldName+"AsString");
		});
	}
	
	public static final List<String> FIELDS_SCOPES_FUNCTIONS = List.of(
			FIELD_CREDIT_MANAGER_HOLDER,FIELD_CREDIT_MANAGER_ASSISTANT
			,FIELD_AUTHORIZING_OFFICER_HOLDER,FIELD_AUTHORIZING_OFFICER_ASSISTANT
			,FIELD_FINANCIAL_CONTROLLER_HOLDER,FIELD_FINANCIAL_CONTROLLER_ASSISTANT
			,FIELD_ACCOUNTING_HOLDER,FIELD_ACCOUNTING_ASSISTANT
		);
	
	public static final String TABLE_NAME = "AFFECTATIONS";
	public static final String AUDIT_TABLE_NAME = TABLE_NAME+"_AUD";
	public static final String STORED_PROCEDURE_QUERY_PROCEDURE_NAME_CLEAN = "P_EFFACER_AFFECTATIONS";
	public static final String STORED_PROCEDURE_QUERY_PROCEDURE_NAME_IMPORT = "P_IMPORTER_AFFECTATIONS";
	public static final String STORED_PROCEDURE_QUERY_PROCEDURE_NAME_IMPORT_NEWS = "P_IMPORTER_AFFECTATIONS_NVL";
	public static final String STORED_PROCEDURE_QUERY_PROCEDURE_NAME_EXPORT = "P_EXPORTER_AFFECTATIONS";
	
	public static final String STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTOR = "audit_acteur";
	public static final String STORED_PROCEDURE_PARAMETER_NAME_AUDIT_FUNCTIONALITY = "audit_fonctionalite";
	public static final String STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTION = "audit_action";
	public static final String STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTION_CREATE = "audit_action_creer";
	public static final String STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTION_UPDATE = "audit_action_modifier";
	public static final String STORED_PROCEDURE_PARAMETER_NAME_AUDIT_DATE = "audit_date";
	
	public static final String COLUMN_IDENTIFIER = "IDENTIFIANT";
	public static final String COLUMN_EXECUTION_IMPUTATION = "IMPUTATION";
	public static final String COLUMN_CREDIT_MANAGER_HOLDER = "GC";
	public static final String COLUMN_CREDIT_MANAGER_ASSISTANT = "A"+COLUMN_CREDIT_MANAGER_HOLDER;
	public static final String COLUMN_AUTHORIZING_OFFICER_HOLDER = "ORD";
	public static final String COLUMN_AUTHORIZING_OFFICER_ASSISTANT = "A"+COLUMN_AUTHORIZING_OFFICER_HOLDER;
	public static final String COLUMN_FINANCIAL_CONTROLLER_HOLDER = "CF";
	public static final String COLUMN_FINANCIAL_CONTROLLER_ASSISTANT = "A"+COLUMN_FINANCIAL_CONTROLLER_HOLDER;
	public static final String COLUMN_ACCOUNTING_HOLDER = "CPT";
	public static final String COLUMN_ACCOUNTING_ASSISTANT = "A"+COLUMN_ACCOUNTING_HOLDER;
	
	public static final String COLUMN___AUDIT_WHO__ = "AUDIT_ACTEUR";
	public static final String COLUMN___AUDIT_FUNCTIONALITY__ = "AUDIT_FONCTIONALITE";
	public static final String COLUMN___AUDIT_WHAT__ = "AUDIT_ACTION";
	public static final String COLUMN___AUDIT_WHEN__ = "AUDIT_DATE";
	
	public static final List<String> COLUMNS_SCOPE_FUNCTIONS_HOLDERS = List.of(
			COLUMN_CREDIT_MANAGER_HOLDER,COLUMN_AUTHORIZING_OFFICER_HOLDER,COLUMN_FINANCIAL_CONTROLLER_HOLDER,COLUMN_ACCOUNTING_HOLDER);
	
	public static final List<String> COLUMNS_SCOPES_FUNCTIONS = List.of(
			COLUMN_CREDIT_MANAGER_HOLDER,COLUMN_CREDIT_MANAGER_ASSISTANT
			,COLUMN_AUTHORIZING_OFFICER_HOLDER,COLUMN_AUTHORIZING_OFFICER_ASSISTANT
			,COLUMN_FINANCIAL_CONTROLLER_HOLDER,COLUMN_FINANCIAL_CONTROLLER_ASSISTANT
			,COLUMN_ACCOUNTING_HOLDER,COLUMN_ACCOUNTING_ASSISTANT
		);
	
	public static String getColumnNameFromFieldName(String fieldName) {
		if(FIELD_CREDIT_MANAGER_HOLDER.equals(fieldName))
			return COLUMN_CREDIT_MANAGER_HOLDER;
		if(FIELD_AUTHORIZING_OFFICER_HOLDER.equals(fieldName))
			return COLUMN_AUTHORIZING_OFFICER_HOLDER;
		if(FIELD_FINANCIAL_CONTROLLER_HOLDER.equals(fieldName))
			return COLUMN_FINANCIAL_CONTROLLER_HOLDER;
		if(FIELD_ACCOUNTING_HOLDER.equals(fieldName))
			return COLUMN_ACCOUNTING_HOLDER;
		throw new RuntimeException(String.format("Column of field named %s not found", fieldName));
	}
}