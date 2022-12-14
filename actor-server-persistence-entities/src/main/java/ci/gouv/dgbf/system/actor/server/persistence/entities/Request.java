package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.lang.reflect.Modifier;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.NamedStoredProcedureQueries;
import javax.persistence.NamedStoredProcedureQuery;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.apache.commons.lang3.StringUtils;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringAuditedImpl;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.persistence.query.EntityFinder;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Request.TABLE_NAME)
@AttributeOverrides(value= {
		@AttributeOverride(name = Request.FIELD___AUDIT_WHO__,column = @Column(name="AUDIT_ACTEUR"))
		,@AttributeOverride(name = Request.FIELD___AUDIT_WHAT__,column = @Column(name="AUDIT_ACTION"))
		,@AttributeOverride(name = Request.FIELD___AUDIT_WHEN__,column = @Column(name="AUDIT_DATE"))
		,@AttributeOverride(name = Request.FIELD___AUDIT_FUNCTIONALITY__,column = @Column(name="AUDIT_FONCTIONALITE"))
})
@NamedStoredProcedureQueries(value = {
		@NamedStoredProcedureQuery(
				name = Request.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_CREATE_USERS, 
				procedureName = Request.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_CREATE_USERS
		)
	})
public class Request extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringAuditedImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	/* Initialization */
	
	@Column(name = COLUMN_BUDGET_CATEGORY_IDENTIFIER,nullable = false) @NotNull private String budgetCategoryIdentifier;
	@Transient private BudgetCategory budgetCategory;
	@Transient private String budgetCategoryAsString;
	
	@ManyToOne @JoinColumn(name = COLUMN_TYPE) @NotNull private RequestType type;
	@Transient private String typeAsString;
	@ManyToOne @JoinColumn(name = COLUMN_STATUS) @NotNull private RequestStatus status;
	@Transient private String statusAsString;
	@Column(name = COLUMN_ACCOUNT_CREATION_DATE) private LocalDateTime accountCreationDate;
	@Transient private String accountCreationDateAsString;
	@Column(name = COLUMN_ACCOUNT_CREATION_MESSAGE) private String accountCreationMessage;
	//@Transient private Boolean isInitializedStatus;
	@Column(name = COLUMN_CREATION_DATE) @NotNull private LocalDateTime creationDate;
	@Transient private String creationDateAsString;
	@Column(name = COLUMN_AUTHENTICATION_REQUIRED) private Boolean authenticationRequired;
	@Transient private String authenticationRequiredAsString;
	@Column(name = COLUMN_ACCESS_TOKEN) private String accessToken;
	
	/* Identity */
	
	@ManyToOne @JoinColumn(name = COLUMN_ACTOR) private Actor actor;
	@Transient private String actorAsString,actorCode,actorNames;
	@Column(name = COLUMN_FIRST_NAME) private String firstName;
	@Column(name = COLUMN_LAST_NAMES) private String lastNames;
	@Transient private String firstNameAndLastNames;
	@Column(name = COLUMN_REGISTRATION_NUMBER) private String registrationNumber;
	@ManyToOne @JoinColumn(name = COLUMN_CIVILITY) /*@Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)*/ private Civility civility;
	@ManyToOne @JoinColumn(name = COLUMN_GROUP) /*@Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)*/ private IdentityGroup group;
	@Column(name = COLUMN_ELECTRONIC_MAIL_ADDRESS) private String electronicMailAddress;
	@Column(name = COLUMN_POSTAL_BOX_ADDRESS) private String postalBoxAddress;
	@Column(name = COLUMN_MOBILE_PHONE_NUMBER) private String mobilePhoneNumber;
	@Column(name = COLUMN_OFFICE_PHONE_NUMBER) private String officePhoneNumber;
	@Column(name = COLUMN_OFFICE_PHONE_EXTENSION) private String officePhoneExtension;
	@Column(name = COLUMN_PHOTO) @Lob private byte[] photo;
	@Transient private String photoIdentifier;
	@Column(name = COLUMN_SIGNATURE) @Lob private byte[] signature;
	@Transient private String signatureIdentifier;
	
	/* Job */
	
	@Column(name = COLUMN_BUDGETARY_EXERCICE) private Integer budgetaryExercice;
	@ManyToOne @JoinColumn(name = COLUMN_ADMINISTRATIVE_UNIT) private AdministrativeUnit administrativeUnit;
	@Transient private String administrativeUnitAsString;
	@Column(name = COLUMN_ADMINISTRATIVE_FUNCTION) private String administrativeFunction;
	@ManyToOne @JoinColumn(name = COLUMN_SECTION) private Section section;
	@Transient private String sectionAsString;
	@ManyToOne @JoinColumn(name = COLUMN_BUDGET_SPECIALIZATION_UNIT) private BudgetSpecializationUnit budgetSpecializationUnit;	
	@Column(name = COLUMN_ACT_OF_APPOINTMENT) @Lob private byte[] actOfAppointment;
	@Transient private String actOfAppointmentIdentifier;
	@Column(name = COLUMN_ACT_OF_APPOINTMENT_REFERENCE) private String actOfAppointmentReference;
	@Column(name = COLUMN_ACT_OF_APPOINTMENT_SIGNATORY) private String actOfAppointmentSignatory;
	@Column(name = COLUMN_ACT_OF_APPOINTMENT_SIGNATURE_DATE) private LocalDate actOfAppointmentSignatureDate;
	@Transient private Long actOfAppointmentSignatureDateAsTimestamp;
	@Transient private String actOfAppointmentSignatureDateAsString;
	
	@Column(name = COLUMN_SIGNED_REQUEST_SHEET) @Lob private byte[] signedRequestSheet;
	@Transient private String signedRequestSheetIdentifier;
	//@Transient private Boolean signedRequestSheetUpdatable;
	
	@Transient private Collection<Function> functions;
	@Transient private Collection<String> functionsAsStrings;
	@Transient private Collection<Function> budgetariesFunctions;
	@Transient private Collection<String> budgetariesFunctionsAsStrings;
	@Transient private Collection<ScopeFunction> budgetariesScopeFunctions,grantedBudgetariesScopeFunctions;
	@Transient private Collection<String> scopeFunctionsCodes,grantedScopeFunctionsCodes;
	@Transient private String scopeFunctionsCodesAsString,grantedScopeFunctionsCodesAsString;
	@Transient private Collection<String> budgetariesScopeFunctionsAsStrings;
	@Transient private Collection<String> budgetariesScopeFunctionsGrantedAsStrings;
	@Transient private Boolean hasGrantedHolderScopeFunction;
	@Transient private Boolean hasBudgetaryScopeFunctionWhereFunctionCodeBelongsToExecutionAssistantsCodes;
	@Transient private Boolean hasBudgetaryScopeFunctionWhereFunctionCodeIsCreditManagerHolder;
	@Transient private Boolean hasBudgetaryScopeFunctionWhereFunctionCodeIsAuthorizingOfficerHolder;
	@Transient private Boolean hasBudgetaryScopeFunctionWhereFunctionCodeIsFinancialControllerHolder;
	@Transient private Boolean hasBudgetaryScopeFunctionWhereFunctionCodeIsAccountingHolder;
	
	/* Others */
	
	@Column(name = COLUMN_COMMENT) private String comment;	
	
	/* Processing */
	
	@Column(name = COLUMN_PROCESSING_DATE) private LocalDateTime processingDate;	
	@Transient private String processingDateAsString;
	@Transient private Boolean processed;
	@Column(name = COLUMN_ACCEPTATION_COMMENT) private String acceptationComment;
	@Transient private Boolean accepted;
	@Column(name = COLUMN_REJECTION_REASON) private String rejectionReason;
	@Transient private Boolean rejected;
	@ManyToOne @JoinColumn(name = COLUMN_DISPATCH_SLIP) private RequestDispatchSlip dispatchSlip;
	@Transient private String dispatchSlipAsString;
	@Transient private String dispatchSlipCode;
	
	/* Links */
	
	@Transient private String readReportURIQuery;
	@Transient private String readPageURL;
	@Transient private String signatureSpecimenReadPageURL;
	
	@Transient private Boolean isAdministrator;
	@Transient private Boolean isCreditManagerHolder;
	@Transient private Boolean isAuthorizingOfficerHolder;
	@Transient private Boolean isFinancialControllerHolder;
	@Transient private Boolean isAccountingHolder;
	@Transient private Boolean isAssistant;
	
	public Collection<String> getScopeFunctionsCodes(Boolean injectIfNull) {
		if(scopeFunctionsCodes == null && Boolean.TRUE.equals(injectIfNull))
			scopeFunctionsCodes = new ArrayList<>();
		return scopeFunctionsCodes;
	}
	
	public Collection<String> getGrantedScopeFunctionsCodes(Boolean injectIfNull) {
		if(grantedScopeFunctionsCodes == null && Boolean.TRUE.equals(injectIfNull))
			grantedScopeFunctionsCodes = new ArrayList<>();
		return grantedScopeFunctionsCodes;
	}
	
	public Collection<String> getBudgetariesScopeFunctionsAsStrings(Boolean injectIfNull) {
		if(budgetariesScopeFunctionsAsStrings == null && Boolean.TRUE.equals(injectIfNull))
			budgetariesScopeFunctionsAsStrings = new ArrayList<>();
		return budgetariesScopeFunctionsAsStrings;
	}
	
	public Collection<String> getBudgetariesScopeFunctionsGrantedAsStrings(Boolean injectIfNull) {
		if(budgetariesScopeFunctionsGrantedAsStrings == null && Boolean.TRUE.equals(injectIfNull))
			budgetariesScopeFunctionsGrantedAsStrings = new ArrayList<>();
		return budgetariesScopeFunctionsGrantedAsStrings;
	}
	
	@Override
	public Request setIdentifier(String identifier) {
		return (Request) super.setIdentifier(identifier);
	}
	
	@Override
	public Request setCode(String code) {
		return (Request) super.setCode(code);
	}
	
	public Request setTypeFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setType(null);
		else
			setType(EntityFinder.getInstance().find(RequestType.class, identifier));
		return this;
	}
	
	public static Map<String,IdentificationAttribute> computeFieldsNames(IdentificationForm form) {
		if(form == null || CollectionHelper.isEmpty(form.getAttributs()) || CollectionHelper.isEmpty(COLUMNS_FIELDS_NAMES))
			return null;
		Map<String,IdentificationAttribute> fieldsNames = new LinkedHashMap<>();
		form.getAttributs().forEach(attribut -> {
			for(String columnFieldName : COLUMNS_FIELDS_NAMES) {
				String columnName = (String) FieldHelper.readStatic(Request.class,columnFieldName);
				if(columnName.equals(attribut.getCode()))
					fieldsNames.put((String) FieldHelper.readStatic(Request.class,"FIELD_"+StringUtils.substringAfter(columnFieldName, "COLUMN_")),attribut);
			}		
		});
		return fieldsNames;
	}
	
	public Boolean hasStatus(String code) {
		if(StringHelper.isBlank(code))
			return Boolean.FALSE;
		return status != null && code.equals(status.getCode());
	}
	
	public void computeHasBudgetaryScopeFunctionWhereFunctionCode(Collection<RequestScopeFunction> requestScopeFunctions) {
		//if(hasBudgetaryScopeFunctionWhereFunctionCodeBelongsToExecutionAssistantsCodes != null)
		//	return hasBudgetaryScopeFunctionWhereFunctionCodeBelongsToExecutionAssistantsCodes;
		if(CollectionHelper.isEmpty(requestScopeFunctions)) {
			hasBudgetaryScopeFunctionWhereFunctionCodeIsCreditManagerHolder = Boolean.FALSE;
			hasBudgetaryScopeFunctionWhereFunctionCodeIsAuthorizingOfficerHolder = Boolean.FALSE;
			hasBudgetaryScopeFunctionWhereFunctionCodeIsFinancialControllerHolder = Boolean.FALSE;
			hasBudgetaryScopeFunctionWhereFunctionCodeIsAccountingHolder = Boolean.FALSE;
			hasBudgetaryScopeFunctionWhereFunctionCodeBelongsToExecutionAssistantsCodes = Boolean.FALSE;
		}else {
			Boolean isHasStatusAccepted = Boolean.TRUE.equals(hasStatus(RequestStatus.CODE_ACCEPTED));
			Collection<RequestScopeFunction> collection = null;
			if(isHasStatusAccepted)
				collection = requestScopeFunctions.stream().filter(x -> Boolean.TRUE.equals(x.getGranted())).collect(Collectors.toList());
			else
				collection = requestScopeFunctions;
			hasBudgetaryScopeFunctionWhereFunctionCodeIsCreditManagerHolder = !collection.stream().filter(x -> x.getScopeFunction().getFunction().isCodeEqualCreditManager())
					.collect(Collectors.toList()).isEmpty();
			hasBudgetaryScopeFunctionWhereFunctionCodeIsAuthorizingOfficerHolder = !collection.stream().filter(x -> x.getScopeFunction().getFunction().isCodeEqualAuthorizingOfficer())
					.collect(Collectors.toList()).isEmpty();
			hasBudgetaryScopeFunctionWhereFunctionCodeIsFinancialControllerHolder = !collection.stream().filter(x -> x.getScopeFunction().getFunction().isCodeEqualFinancialController())
					.collect(Collectors.toList()).isEmpty();
			hasBudgetaryScopeFunctionWhereFunctionCodeIsAccountingHolder = !collection.stream().filter(x -> x.getScopeFunction().getFunction().isCodeEqualAccounting())
					.collect(Collectors.toList()).isEmpty();
			hasBudgetaryScopeFunctionWhereFunctionCodeBelongsToExecutionAssistantsCodes = !collection.stream().filter(x -> x.getScopeFunction().getFunction().isCodeBelongsToExecutionAssisantsCodes())
					.collect(Collectors.toList()).isEmpty();
		}
	}
	
	public static final String FIELD_TYPE = "type";
	public static final String FIELD_TYPE_AS_STRING = "typeAsString";
	public static final String FIELD_STATUS = "status";
	public static final String FIELD_STATUS_AS_STRING = "statusAsString";
	public static final String FIELD_ACCOUNT_CREATION_DATE = "accountCreationDate";
	public static final String FIELD_ACCOUNT_CREATION_DATE_AS_STRING = "accountCreationDateAsString";
	public static final String FIELD_ACCOUNT_CREATION_MESSAGE = "accountCreationMessage";
	public static final String FIELD_ACCEPTED = "accepted";
	public static final String FIELD_REJECTED = "rejected";
	public static final String FIELD_PROCESSED = "processed";
	public static final String FIELD_CREATION_DATE = "creationDate";
	public static final String FIELD_CREATION_DATE_AS_STRING = "creationDateAsString";
	public static final String FIELD_AUTHENTICATION_REQUIRED = "authenticationRequired";
	public static final String FIELD_AUTHENTICATION_REQUIRED_AS_STRING = "authenticationRequiredAsString";
	public static final String FIELD_ACCESS_TOKEN = "accessToken";
	
	public static final String FIELD_ACTOR = "actor";
	public static final String FIELD_ACTOR_CODE = "actorCode";
	public static final String FIELD_ACTOR_NAMES = "actorNames";
	public static final String FIELD_ACTOR_AS_STRING = "actorAsString";
	public static final String FIELD_GROUP = "group";
	public static final String FIELD_CIVILITY = "civility";
	public static final String FIELD_FIRST_NAME = "firstName";
	public static final String FIELD_LAST_NAMES = "lastNames";
	public static final String FIELD_REGISTRATION_NUMBER = "registrationNumber";
	public static final String FIELD_MOBILE_PHONE_NUMBER = "mobilePhoneNumber";
	public static final String FIELD_OFFICE_PHONE_NUMBER = "officePhoneNumber";
	public static final String FIELD_OFFICE_PHONE_EXTENSION = "officePhoneExtension";
	public static final String FIELD_POSTAL_BOX_ADDRESS = "postalBoxAddress";
	public static final String FIELD_ELECTRONIC_MAIL_ADDRESS = "electronicMailAddress";
	public static final String FIELD_PHOTO = "photo";
	public static final String FIELD_SIGNATURE = "signature";
	public static final String FIELD_FIRST_NAME_AND_LAST_NAMES = "firstNameAndLastNames";
	
	public static final String FIELD_BUDGETARY_EXERCICE = "budgetaryExercice";	
	public static final String FIELD_ADMINISTRATIVE_UNIT = "administrativeUnit";
	public static final String FIELD_ADMINISTRATIVE_UNIT_AS_STRING = "administrativeUnitAsString";
	public static final String FIELD_SECTION_AS_STRING = "sectionAsString";
	public static final String FIELD_ADMINISTRATIVE_FUNCTION = "administrativeFunction";
	public static final String FIELD_SECTION = "section";
	public static final String FIELD_BUDGET_SPECIALIZATION_UNIT = "budgetSpecializationUnit";
	public static final String FIELD_ACT_OF_APPOINTMENT = "actOfAppointment";
	public static final String FIELD_ACT_OF_APPOINTMENT_REFERENCE = "actOfAppointmentReference";
	public static final String FIELD_ACT_OF_APPOINTMENT_SIGNATORY = "actOfAppointmentSignatory";
	public static final String FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE = "actOfAppointmentSignatureDate";
	public static final String FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE_AS_TIMESTAMP = "actOfAppointmentSignatureDateAsTimestamp";
	public static final String FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE_AS_STRING = "actOfAppointmentSignatureDateAsString";
	public static final String FIELD_FUNCTIONS = "functions";
	public static final String FIELD_FUNCTIONS_AS_STRINGS = "functionsAsStrings";
	public static final String FIELD_SIGNED_REQUEST_SHEET = "signedRequestSheet";
	
	public static final String FIELD_GRANTED_BUDGETARIES_SCOPE_FUNCTIONS = "grantedBudgetariesScopeFunctions";
	public static final String FIELD_BUDGETARIES_SCOPE_FUNCTIONS_AS_STRINGS = "budgetariesScopeFunctionsAsStrings";
	public static final String FIELD_BUDGETARIES_SCOPE_FUNCTIONS_GRANTED_AS_STRINGS = "budgetariesScopeFunctionsGrantedAsStrings";
	
	public static final String FIELD_COMMENT = "comment";
	
	public static final String FIELD_PROCESSING_DATE = "processingDate";
	public static final String FIELD_PROCESSING_DATE_AS_STRING = "processingDateAsString";
	public static final String FIELD_ACCEPTATION_COMMENT = "acceptationComment";
	public static final String FIELD_REJECTION_REASON = "rejectionReason";
	public static final String FIELD_DISPATCH_SLIP = "dispatchSlip";
	public static final String FIELD_DISPATCH_SLIP_AS_STRING = "dispatchSlipAsString";
	public static final String FIELD_DISPATCH_SLIP_CODE = "dispatchSlipCode";
	
	public static final String FIELD_SCOPE_FUNCTIONS_CODES_AS_STRING = "scopeFunctionsCodesAsString";
	public static final String FIELD_GRANTED_SCOPE_FUNCTIONS_CODES_AS_STRING = "grantedScopeFunctionsCodesAsString";
	
	public static final String FIELD_HAS_GRANTED_HOLDER_SCOPE_FUNCTION = "hasGrantedHolderScopeFunction";
	
	public static final String FIELD_IS_CREDIT_MANAGER_HOLDER = "isCreditManagerHolder";
	public static final String FIELD_IS_AUTHORIZING_OFFICER_HOLDER = "isAuthorizingOfficerHolder";
	public static final String FIELD_IS_FINANCIAL_CONTROLLER_HOLDER = "isFinancialControllerHolder";
	public static final String FIELD_IS_ACCOUNTING_HOLDER = "isAccountingHolder";
	public static final String FIELDS_IS_CREDIT_MANAGER_HOLDER_IS_AUTHORIZING_OFFICER_HOLDER_IS_FINANCIAL_CONTROLLER_HOLDER_IS_ACCOUNTING_HOLDER = "isCreditManagerHolderisAuthorizingOfficerHolderisFinancialControllerHolderisAccountingHolder";
	
	public static final String FIELDS_SECTION_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS = "sectionAdministrativeUnitTypeStatusCreationDateProcessingDateAsStrings";
	public static final String FIELDS_SECTION_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_AS_STRINGS = "sectionAdministrativeUnitTypeStatusCreationDateAsStrings";
	public static final String FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS = "sectionAsCodeAdministrativeUnitTypeStatusCreationDateProcessingDateAsStrings";
	public static final String FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_AS_STRINGS = "sectionAsCodeAdministrativeUnitTypeStatusCreationDateAsStrings";
	public static final String FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_AS_CODE_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS = "sectionAsCodeAdministrativeUnitAsCodeTypeStatusCreationDateProcessingDateAsStrings";
	public static final String FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_AS_CODE_TYPE_STATUS_CREATION_DATE_AS_STRINGS = "sectionAsCodeAdministrativeUnitAsCodeTypeStatusCreationDateAsStrings";	
	public static final String FIELDS_SCOPE_FUNCTIONS_CODES = "scopeFunctionsCodes";
	public static final String FIELDS_SCOPE_FUNCTIONS_CODES_IS_CREDIT_MANAGER_HOLDER_IS_AUTHORIZING_OFFICER_HOLDER_IS_FINANCIAL_CONTROLLER_HOLDER_IS_ACCOUNTING_HOLDER = "scopeFunctionsCodesIsCreditManagerHolderisAuthorizingOfficerHolderisFinancialControllerHolderisAccountingHolder";
	public static final String FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES = "grantedScopeFunctionsCodes";
	public static final String FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES_IS_CREDIT_MANAGER_HOLDER_IS_AUTHORIZING_OFFICER_HOLDER_IS_FINANCIAL_CONTROLLER_HOLDER_IS_ACCOUNTING_HOLDER = "grantedScopeFunctionsCodesIsCreditManagerHolderisAuthorizingOfficerHolderisFinancialControllerHolderisAccountingHolder";
	
	public static final String TABLE_NAME = "DM_DEMANDE";
	public static final String STORED_PROCEDURE_QUERY_PROCEDURE_NAME_CREATE_USERS = "CREATION_ACTEUR";
	
	public static final String COLUMN_BUDGET_CATEGORY_IDENTIFIER = "CATEGORIE_BUDGET";
	public static final String COLUMN_IDENTIFIER = "IDENTIFIANT";
	public static final String COLUMN_TYPE = "TYPE";
	public static final String COLUMN_STATUS = "STATUT";
	public static final String COLUMN_ACCOUNT_CREATION_DATE = "DATE_CREATION_COMPTE";
	public static final String COLUMN_ACCOUNT_CREATION_MESSAGE = "MESSAGE_CREATION_COMPTE";
	public static final String COLUMN_CREATION_DATE = "DATE_CREATION";
	public static final String COLUMN_AUTHENTICATION_REQUIRED = "AUTHENTIFICATION_REQUISE";
	public static final String COLUMN_ACCESS_TOKEN = "jeton_acces";
	
	public static final String COLUMN_ACTOR = "ACTEUR";
	public static final String COLUMN_GROUP = "GROUPE";
	public static final String COLUMN_CIVILITY = "CIVILITE";
	public static final String COLUMN_FIRST_NAME = "NOM";
	public static final String COLUMN_LAST_NAMES = "PRENOMS";	
	public static final String COLUMN_REGISTRATION_NUMBER = "MATRICULE";
	public static final String COLUMN_ELECTRONIC_MAIL_ADDRESS = "EMAIL";
	public static final String COLUMN_POSTAL_BOX_ADDRESS = "BOITE_POSTALE";
	public static final String COLUMN_MOBILE_PHONE_NUMBER = "NUMERO_MOBILE";
	public static final String COLUMN_OFFICE_PHONE_NUMBER = "NUMERO_BUREAU";
	public static final String COLUMN_OFFICE_PHONE_EXTENSION = "POSTE_BUREAU";
	public static final String COLUMN_PHOTO = "PHOTO";
	public static final String COLUMN_SIGNATURE = "SIGNATURE";
	
	public static final String COLUMN_BUDGETARY_EXERCICE = "EXERCICE_BUDGETAIRE";
	public static final String COLUMN_ADMINISTRATIVE_UNIT = "UNITE_ADMINISTRATIVE";
	public static final String COLUMN_ADMINISTRATIVE_FUNCTION = "FONCTION_ADMINISTRATIVE";
	public static final String COLUMN_ACT_OF_APPOINTMENT = "ACTE_NOMINATION";
	public static final String COLUMN_ACT_OF_APPOINTMENT_REFERENCE = "REFERENCE_ACTE_NOMINATION";
	public static final String COLUMN_ACT_OF_APPOINTMENT_SIGNATORY = "SIGNATAIRE_ACTE_NOMINATION";
	public static final String COLUMN_ACT_OF_APPOINTMENT_SIGNATURE_DATE = "DATE_SIGNATURE_ACTE_NOMINATION";
	public static final String COLUMN_SECTION = "SECTION";
	public static final String COLUMN_BUDGET_SPECIALIZATION_UNIT = "USB";
	public static final String COLUMN_SIGNED_REQUEST_SHEET = "FICHE_DEMANDE_SIGNEE";
	
	public static final String COLUMN_COMMENT = "COMMENTAIRE";
	
	public static final String COLUMN_PROCESSING_DATE = "DATE_TRAITEMENT";
	public static final String COLUMN_ACCEPTATION_COMMENT = "COMMENTAIRE_ACCEPTATION";
	public static final String COLUMN_REJECTION_REASON = "MOTIF_REJET";
	public static final String COLUMN_DISPATCH_SLIP = "BORDEREAU";
	
	public static final String LABEL = "Demande";
	
	public static final Collection<String> COLUMNS_FIELDS_NAMES = new ArrayList<>();
	static {
		Collection<String> names = FieldHelper.getNames(FieldHelper.filter(Request.class, "^COLUMN_", Modifier.STATIC));
		if(CollectionHelper.isNotEmpty(names))
			COLUMNS_FIELDS_NAMES.addAll(names);
	}
}