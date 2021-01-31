package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.util.Collection;
import java.util.List;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierGetter;
import org.cyk.utility.__kernel__.persistence.query.QueryName;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AuthorizingOfficerServiceQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ClusterPrivilegesQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ClusterQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.IdentificationFormAttributeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RejectedAccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestDispatchSlipQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeActivityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.SectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationForm;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationFormAttribute;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RejectedAccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeTypeFunction;

public class PersistenceApiUnitTestDev extends AbstractPersistenceApiUnitTestValidate {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Test
	public void administrativeUnit_readWhereCodeOrNameLike(){
		System.out.println(AdministrativeUnitQuerier.getInstance().readWhereCodeOrNameLike(new QueryExecutorArguments()
				.setQueryFromIdentifier(QueryIdentifierGetter.getInstance().get(AdministrativeUnit.class, QueryName.READ_WHERE_CODE_OR_NAME_LIKE))));
	}
	
	@Test
	public void section_readVisibles_christian(){
		System.out.println(SectionQuerier.getInstance().readVisiblesByActorCodeForUI("christian"));
	}
	
	@Test
	public void requestDispatchSlip_readWhereFilterForUI(){
		Collection<RequestDispatchSlip> requestDispatchSlips = RequestDispatchSlipQuerier.getInstance().readWhereFilterForUI(null);
		RequestDispatchSlip requestDispatchSlip = CollectionHelper.getFirst(requestDispatchSlips);
		System.out.println(requestDispatchSlip.getIdentifier());
		System.out.println(requestDispatchSlip.getCode());
		System.out.println(requestDispatchSlip.getName());
		System.out.println(requestDispatchSlip.getFunctionAsString());
		System.out.println(requestDispatchSlip.getCreationDateAsString());
		System.out.println(requestDispatchSlip.getSendingDateAsString());
		System.out.println(requestDispatchSlip.getProcessingDateAsString());
	}
	
	@Test
	public void scopeFunction_readAllWithReferencesOnly(){
		//ScopeFunctionQuerier.getInstance().readAllWithReferencesOnly(new QueryExecutorArguments())
		//.forEach(x-> System.out.println(x.getScopeCode()+" - "+x.getLocalityCode()));
		
		AuthorizingOfficerServiceQuerier.getInstance().readAllForAssignmentsInitialization()
		.forEach(x-> System.out.println(x.getIdentifier()+" - "+x.getBudgetSpecializationUnitCode()+" - "+x.getLocalityCode()));
	}
	
	@Test
	public void form(){
		System.out.println(EntityReader.getInstance().readMany(IdentificationForm.class, new QueryExecutorArguments()
				.setQueryFromIdentifier(QueryIdentifierGetter.getInstance().get(IdentificationForm.class, QueryName.READ))));
		System.out.println(EntityCounter.getInstance().count(IdentificationForm.class, new QueryExecutorArguments()
				.setQueryFromIdentifier(QueryIdentifierGetter.getInstance().get(IdentificationForm.class, QueryName.COUNT))));
		System.out.println(EntityReader.getInstance().readMany(IdentificationForm.class, new QueryExecutorArguments()
				.setQueryFromIdentifier(QueryIdentifierGetter.getInstance().get(IdentificationForm.class, QueryName.READ_FOR_UI))));
		System.out.println(EntityReader.getInstance().readMany(IdentificationForm.class, new QueryExecutorArguments()
				.setQueryFromIdentifier(QueryIdentifierGetter.getInstance().get(IdentificationForm.class, QueryName.READ_BY_IDENTIFIER_FOR_UI))
				.addFilterField("identifier", "DH_GESTIONNAIRE_CREDIT")));
		System.out.println(EntityReader.getInstance().readMany(IdentificationForm.class, new QueryExecutorArguments()
				.setQueryFromIdentifier(QueryIdentifierGetter.getInstance().get(IdentificationForm.class, QueryName.READ_BY_IDENTIFIER_FOR_EDIT))
				.addFilterField("identifier", "DH_GESTIONNAIRE_CREDIT")));
		
		System.out.println("----------------------------------------------------------------------------------------------------------------------");
		
		System.out.println(EntityReader.getInstance().readMany(IdentificationFormAttribute.class, new QueryExecutorArguments()
				.setQueryFromIdentifier(QueryIdentifierGetter.getInstance().get(IdentificationFormAttribute.class, QueryName.READ_WHERE_FILTER))));
		System.out.println(EntityReader.getInstance().readMany(IdentificationFormAttribute.class, new QueryExecutorArguments()
				.setQueryFromIdentifier(QueryIdentifierGetter.getInstance().get(IdentificationFormAttribute.class, QueryName.READ_WHERE_FILTER_FOR_UI))));
		
		System.out.println(EntityReader.getInstance().readMany(IdentificationFormAttribute.class, new QueryExecutorArguments()
				.setQueryFromIdentifier(QueryIdentifierGetter.getInstance().get(IdentificationFormAttribute.class, QueryName.READ_WHERE_FILTER_FOR_UI))
				.addFilterField(IdentificationFormAttributeQuerier.PARAMETER_NAME_FORM_IDENTIFIER, "DH_GESTIONNAIRE_CREDIT")));
		System.out.println(EntityReader.getInstance().readMany(IdentificationFormAttribute.class, new QueryExecutorArguments()
				.setQueryFromIdentifier(QueryIdentifierGetter.getInstance().get(IdentificationFormAttribute.class, QueryName.READ_WHERE_FILTER_FOR_UI))
				.addFilterField(IdentificationFormAttributeQuerier.PARAMETER_NAME_FORM_IDENTIFIER, "1")));
		
		System.out.println(EntityReader.getInstance().readMany(IdentificationFormAttribute.class, new QueryExecutorArguments()
				.setQueryFromIdentifier(QueryIdentifierGetter.getInstance().get(IdentificationFormAttribute.class, QueryName.READ_WHERE_FILTER_FOR_EDIT))
				.addFilterField(IdentificationFormAttributeQuerier.PARAMETER_NAME_FORM_IDENTIFIER, "DH_GESTIONNAIRE_CREDIT")));
		
		System.out.println(EntityReader.getInstance().readMany(IdentificationFormAttribute.class, new QueryExecutorArguments()
				.setQueryFromIdentifier(QueryIdentifierGetter.getInstance().get(IdentificationFormAttribute.class, QueryName.READ_WHERE_FILTER_FOR_EDIT))
				.addFilterField(IdentificationFormAttributeQuerier.PARAMETER_NAME_FORM_IDENTIFIER, "DH_GESTIONNAIRE_CREDIT")).iterator().next().getForm());
		
		System.out.println(EntityReader.getInstance().readMany(IdentificationFormAttribute.class, new QueryExecutorArguments()
				.setQueryFromIdentifier(QueryIdentifierGetter.getInstance().get(IdentificationFormAttribute.class, QueryName.READ_WHERE_FILTER_FOR_UI))
				.addFilterField(IdentificationFormAttributeQuerier.PARAMETER_NAME_FORM_IDENTIFIER, "DH_GESTIONNAIRE_CREDIT")).iterator().next().getRequiredAsString());
	}
	
	@Test
	public void actor_instantiateOneToBeCreatedByPublic(){
		Actor actor = ActorQuerier.getInstance().instantiateOneToBeCreatedByPublic();
		System.out.println(actor);
		System.out.println(actor.getForm().getAttributs());
	}
	
	@Test
	public void actor_readByIdentifierForEdit(){
		Actor actor = EntityReader.getInstance().readOne(Actor.class, new QueryExecutorArguments()
				.setQueryFromIdentifier(ActorQuerier.QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT).addFilterFieldsValues(ActorQuerier.PARAMETER_NAME_IDENTIFIER,"ACT_christian"));
		System.out.println(actor.getIdentifier());
		System.out.println(actor.getCode());
		System.out.println(actor.getCivility());
		System.out.println(actor.getFirstName());
		System.out.println(actor.getLastNames());
		System.out.println(actor.getRegistrationNumber());
		
		System.out.println(actor.getMobilePhoneNumber());
		System.out.println(actor.getOfficePhoneNumber());
		System.out.println(actor.getOfficePhoneExtension());
		System.out.println(actor.getElectronicMailAddress());
		System.out.println(actor.getPostalBoxAddress());
		
		System.out.println(actor.getAdministrativeUnit());
		System.out.println(actor.getAdministrativeFunction());
		
		System.out.println(actor.getActOfAppointmentReference());
		System.out.println(actor.getActOfAppointmentSignatureDateAsTimestamp());
		System.out.println(actor.getActOfAppointmentSignatory());
	}
	
	@Test
	public void actor_readProfileInformationsByIdentifier(){
		Actor actor = ActorQuerier.getInstance().readProfileInformationsByIdentifierForUI("ACT_christian");
		System.out.println(actor.getIdentifier());
		System.out.println(actor.getCode());
		System.out.println(actor.getCivilityAsString());
		System.out.println(actor.getFirstName());
		System.out.println(actor.getLastNames());
		System.out.println(actor.getNames());
		System.out.println(actor.getRegistrationNumber());
		
		System.out.println(actor.getMobilePhoneNumber());
		System.out.println(actor.getOfficePhoneNumber());
		System.out.println(actor.getOfficePhoneExtension());
		System.out.println(actor.getElectronicMailAddress());
		System.out.println(actor.getPostalBoxAddress());
		
		System.out.println(actor.getSectionAsString());
		System.out.println(actor.getAdministrativeUnitAsString());
		System.out.println(actor.getAdministrativeFunction());
		
		System.out.println(actor.getActOfAppointmentReference());
		System.out.println(actor.getActOfAppointmentSignatureDateAsString());
		System.out.println(actor.getActOfAppointmentSignatory());
	}
	
	@Test
	public void request(){
		System.out.println(RequestTypeQuerier.getInstance().readAll());
		System.out.println(RequestTypeQuerier.getInstance().readForUI(new QueryExecutorArguments()));
		System.out.println(RequestTypeQuerier.getInstance().countForUI(new QueryExecutorArguments()));
		System.out.println(RequestTypeQuerier.getInstance().readByIdentifierForRequestCreation("DM_GESTIONNAIRE_CREDIT"));
		System.out.println(RequestTypeQuerier.getInstance().readByIdentifierForRequestCreation("DM_GESTIONNAIRE_CREDIT").getForm());
		System.out.println(RequestTypeQuerier.getInstance().readByIdentifierForRequestCreation("DM_GESTIONNAIRE_CREDIT").getForm().getAttributs());
		
		System.out.println("-------------------------- instanciate by type -----------------------------");
		Request request = EntityReader.getInstance().readOne(Request.class, new QueryExecutorArguments().setQuery(new Query()
				.setIdentifier(RequestQuerier.QUERY_IDENTIFIER_INSTANTIATE_ONE_BY_TYPE_IDENTIFIER))
				.addFilterFieldsValues(RequestQuerier.PARAMETER_NAME_TYPE_IDENTIFIER,"DM_GESTIONNAIRE_CREDIT"));
		System.out.println(request.getType());
		System.out.println(request.getType().getForm());
		System.out.println(request.getType().getForm().getAttributs());
		
		System.out.println("-------------------------- instanciate by type by actor -----------------------------");
		request = EntityReader.getInstance().readOne(Request.class, new QueryExecutorArguments().setQuery(new Query()
				.setIdentifier(RequestQuerier.QUERY_IDENTIFIER_INSTANTIATE_ONE_BY_TYPE_IDENTIFIER_BY_ACTOR_IDENTIFIER))
				.addFilterFieldsValues(RequestQuerier.PARAMETER_NAME_TYPE_IDENTIFIER,"DM_GESTIONNAIRE_CREDIT"
						,RequestQuerier.PARAMETER_NAME_ACTOR_IDENTIFIER,"ACT_christian"));
		System.out.println(request.getAdministrativeUnit());
		System.out.println(request.getAdministrativeFunction());
		System.out.println(request.getActOfAppointmentReference());
		System.out.println(request.getActOfAppointmentSignatory());
		System.out.println(request.getActOfAppointmentSignatureDate());
		System.out.println(request.getActOfAppointmentSignatureDateAsTimestamp());
		
		System.out.println("-------------------------- read by identifier for UI -----------------------------");
		request = EntityReader.getInstance().readOne(Request.class, new QueryExecutorArguments().setQuery(new Query().setIdentifier(RequestQuerier.QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI))
				.addFilterFieldsValues(RequestQuerier.PARAMETER_NAME_IDENTIFIER,"1"));
		System.out.println(request.getAdministrativeFunction());
		System.out.println(request.getAdministrativeUnit());
		System.out.println(request.getActOfAppointmentSignatureDate());
		System.out.println(request.getActOfAppointmentSignatureDateAsString());
		System.out.println(request.getActOfAppointmentSignatureDateAsTimestamp());
		System.out.println(request.getComment());
		System.out.println(request.getType());
		System.out.println(request.getType().getForm());
		System.out.println(request.getType().getForm().getAttributs());
		System.out.println(Request.computeFieldsNames(request.getType().getForm()));
	}
	
	@Test
	public void request_readWhereFilterForUI(){
		Collection<Request> requests = RequestQuerier.getInstance().readWhereFilterForUI(new QueryExecutorArguments()
				.setQueryFromIdentifier(RequestQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI)
				.addFilterFieldsValues(RequestQuerier.PARAMETER_NAME_EXCLUDED_IDENTIFIERS,null));
		System.out.println("PersistenceApiUnitTestDev.request_readWhereFilterForUI() COUNT : "+CollectionHelper.getSize(requests));
		Request request = CollectionHelper.getFirst(requests);
		System.out.println(request.getActorCode());
		System.out.println(request.getActorNames());
		System.out.println(request.getTypeAsString());
		System.out.println(request.getComment());
		System.out.println(request.getCreationDateAsString());
		System.out.println(request.getBudgetariesScopeFunctionsAsStrings());
		System.out.println(request.getAdministrativeUnitAsString());
		System.out.println(request.getMobilePhoneNumber());
	}
	
	@Test
	public void request_readWhereFilterForUI_all_duration(){
		System.out.println("Reading...");
		Long t = System.currentTimeMillis();
		Collection<Request> requests = RequestQuerier.getInstance().readWhereFilterForUI(new QueryExecutorArguments()
				.setQueryFromIdentifier(RequestQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI));
		System.out.println(requests.size()+" - "+TimeHelper.formatDuration(System.currentTimeMillis() - t));
	}
	
	@Test
	public void request_readWhereFilterForUI_section(){
		Collection<Request> requests = RequestQuerier.getInstance().readWhereFilterForUI(new QueryExecutorArguments()
				.setQueryFromIdentifier(RequestQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI)
				.addFilterFieldsValues(RequestQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT_SECTION_IDENTIFIER,"SECTIONf07cb924-5838-40c2-9de9-13084501d91e"));
		System.out.println("PersistenceApiUnitTestDev.request_readWhereFilterForUI() COUNT : "+CollectionHelper.getSize(requests));
		Request request = CollectionHelper.getFirst(requests);
		System.out.println(request.getActorCode());
		System.out.println(request.getActorNames());
		System.out.println(request.getTypeAsString());
		System.out.println(request.getComment());
		System.out.println(request.getCreationDateAsString());
		System.out.println(request.getBudgetariesScopeFunctionsAsStrings());
		System.out.println(request.getAdministrativeUnitAsString());
		System.out.println(request.getMobilePhoneNumber());
	}
	
	@Test
	public void request_readByIdentifierForUI(){
		Request request = RequestQuerier.getInstance().readByIdentifierForUI("4f5ec170-0419-472e-b9a4-7bca32482610");
		System.out.println(request.getActorCode());
		System.out.println(request.getActorNames());
		System.out.println(request.getStatusAsString());
		System.out.println(request.getTypeAsString());
		System.out.println(request.getComment());
		System.out.println(request.getCreationDateAsString());
		System.out.println(request.getBudgetariesScopeFunctionsAsStrings());
		System.out.println(request.getBudgetariesScopeFunctionsGrantedAsStrings());
		System.out.println(request.getHasBudgetaryScopeFunctionWhereFunctionCodeIsCreditManagerHolder());
		System.out.println(request.getHasBudgetaryScopeFunctionWhereFunctionCodeIsAuthorizingOfficerHolder());
		System.out.println(request.getHasBudgetaryScopeFunctionWhereFunctionCodeIsFinancialControllerHolder());
		System.out.println(request.getHasBudgetaryScopeFunctionWhereFunctionCodeIsAccountingHolder());
	}
	
	@Test
	public void request_readPhotoByIdentifier(){
		byte[] bytes = RequestQuerier.getInstance().readPhotoByIdentifier("5843623c-eda1-4143-bb53-76aa893bd5d6");
		System.out.println("Bytes count : "+(bytes == null ? 0 : bytes.length));
	}
	
	@Test
	public void request_readSignatureByIdentifier(){
		byte[] bytes = RequestQuerier.getInstance().readSignatureByIdentifier("5843623c-eda1-4143-bb53-76aa893bd5d6");
		System.out.println("Bytes count : "+(bytes == null ? 0 : bytes.length));
	}
	
	@Test
	public void clusters_readWhereFilter(){
		System.out.println(ClusterQuerier.getInstance().readWhereFilter(new QueryExecutorArguments()));
	}
	
	@Test
	public void clusterPrivileges_readWhereFilter(){
		System.out.println(ClusterPrivilegesQuerier.getInstance().readWhereFilterForUI(new QueryExecutorArguments()));
	}
	
	@Test
	public void assignments_readWhereFilter_ua13010222(){
		Collection<Assignments> collection = 
				AssignmentsQuerier.getInstance().readWhereFilter(new QueryExecutorArguments().addFilterField(AssignmentsQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT, "13010222"));
		System.out.println("Size : "+CollectionHelper.getSize(collection));
		if(collection != null)
			collection.forEach(x -> {
				System.out.println(x.getExecutionImputation().getActivityCode()+" - "+x.getExecutionImputation().getEconomicNatureCode());
			});
	}
	
	@Test
	public void assignments_readFullyAssignedWhereFilter_ua13010222(){
		Collection<Assignments> collection = 
				AssignmentsQuerier.getInstance().readFullyAssignedWhereFilter(new QueryExecutorArguments().addFilterField(AssignmentsQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT, "13010222"));
		System.out.println("Size : "+CollectionHelper.getSize(collection));
		if(collection != null)
			collection.forEach(x -> {
				System.out.println(x.getExecutionImputation().getActivityCode()+" - "+x.getExecutionImputation().getEconomicNatureCode());
			});
	}
	
	@Test
	public void assignments_readFullyAssignedWhereFilter_readNotFullyAssignedWhereFilter_ua13010222(){
		Collection<Assignments> collection = 
				AssignmentsQuerier.getInstance().readFullyAssignedWhereFilter(new QueryExecutorArguments().addFilterField(AssignmentsQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT, "13010222"));
		System.out.println("Full Size : "+CollectionHelper.getSize(collection));
		if(collection != null)
			collection.forEach(x -> {
				System.out.println(x.getExecutionImputation().getActivityCode()+" - "+x.getExecutionImputation().getEconomicNatureCode());
			});
		collection = 
				AssignmentsQuerier.getInstance().readNotFullyAssignedWhereFilter(new QueryExecutorArguments().addFilterField(AssignmentsQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT, "13010222"));
		System.out.println("Not Full Size : "+CollectionHelper.getSize(collection));
		if(collection != null)
			collection.forEach(x -> {
				System.out.println(x.getExecutionImputation().getActivityCode()+" - "+x.getExecutionImputation().getEconomicNatureCode());
			});
	}
	
	@Test
	public void assignments_readFullyAssignedWhereFilterForUI_readNotFullyAssignedWhereFilterForUI_ua13010222(){
		Collection<Assignments> collection = 
				AssignmentsQuerier.getInstance().readFullyAssignedWhereFilterForUI(new QueryExecutorArguments().addFilterField(AssignmentsQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT, "13010222"));
		System.out.println("Full Size : "+CollectionHelper.getSize(collection));
		if(collection != null)
			collection.forEach(x -> {
				System.out.println(x.getExecutionImputation().getActivityCode()+" - "+x.getExecutionImputation().getEconomicNatureCode());
			});
		collection = 
				AssignmentsQuerier.getInstance().readNotFullyAssignedWhereFilterForUI(new QueryExecutorArguments().addFilterField(AssignmentsQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT, "13010222"));
		System.out.println("Not Full Size : "+CollectionHelper.getSize(collection));
		if(collection != null)
			collection.forEach(x -> {
				System.out.println(x.getActivityAsString()+" - "+x.getEconomicNatureAsString());
			});
	}
	
	@Test
	public void assignments_countWhereFilter(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		System.out.println("Toutes les lignes");
		System.out.println(AssignmentsQuerier.getInstance().countWhereFilter(new QueryExecutorArguments()));
		System.out.println("Les lignes avec tout les titulaires définis");
		System.out.println(AssignmentsQuerier.getInstance().countWhereFilter(new QueryExecutorArguments()
				.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ALL_HOLDERS_DEFINED,Boolean.TRUE)));
		System.out.println("Les lignes avec des titulaires non définis");
		System.out.println(AssignmentsQuerier.getInstance().countWhereFilter(new QueryExecutorArguments()
				.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_SOME_HOLDERS_NOT_DEFINED,Boolean.TRUE)));
	}
	
	@Test
	public void assignments_readWhereFilter(){
		Collection<Assignments> collection = 
				AssignmentsQuerier.getInstance().readWhereFilter(new QueryExecutorArguments().setNumberOfTuples(1));
		if(collection == null)
			return;
		Assignments assignments = collection.iterator().next();
		System.out.println(assignments.getExecutionImputation().getIdentifier());
		System.out.println(assignments.getExecutionImputation().getCode());
	}
	
	@Test
	public void assignments_readByIdentifierForEdit(){
		Assignments assignments = AssignmentsQuerier.getInstance().readByIdentifierForEdit("00009afd-f1a0-4291-a9a1-781e2faaccea");
		System.out.println(assignments.getIdentifier());
		System.out.println(assignments.getSectionAsString());
		System.out.println(assignments.getBudgetSpecializationUnitAsString());
		System.out.println(assignments.getActionAsString());
		System.out.println(assignments.getActivityAsString());
		System.out.println(assignments.getActivityCategoryAsString());
		System.out.println(assignments.getExpenditureNatureAsString());
		System.out.println(assignments.getEconomicNatureAsString());
		System.out.println(assignments.getAdministrativeUnitAsString());
		System.out.println(assignments.getCreditManagerHolder());
		System.out.println(assignments.getCreditManagerAssistant());
		System.out.println(assignments.getAuthorizingOfficerHolder());
		System.out.println(assignments.getAuthorizingOfficerAssistant());
		System.out.println(assignments.getFinancialControllerHolder());
		System.out.println(assignments.getFinancialControllerAssistant());
		System.out.println(assignments.getAccountingHolder());
		System.out.println(assignments.getAccountingAssistant());
	}
	
	@Test
	public void assignments_readWhereFilterForEdit(){
		Collection<Assignments> collection = 
				AssignmentsQuerier.getInstance().readWhereFilterForEdit(new QueryExecutorArguments().setNumberOfTuples(1));
		Assignments assignments = collection.iterator().next();
		System.out.println(assignments.getIdentifier());
		System.out.println(assignments.getSectionAsString());
		System.out.println(assignments.getBudgetSpecializationUnitAsString());
		System.out.println(assignments.getActionAsString());
		System.out.println(assignments.getActivityAsString());
		System.out.println(assignments.getActivityCategoryAsString());
		System.out.println(assignments.getExpenditureNatureAsString());
		System.out.println(assignments.getEconomicNatureAsString());
		System.out.println(assignments.getAdministrativeUnitAsString());
		System.out.println(assignments.getCreditManagerHolder());
		System.out.println(assignments.getCreditManagerAssistant());
		System.out.println(assignments.getAuthorizingOfficerHolder());
		System.out.println(assignments.getAuthorizingOfficerAssistant());
		System.out.println(assignments.getFinancialControllerHolder());
		System.out.println(assignments.getFinancialControllerAssistant());
		System.out.println(assignments.getAccountingHolder());
		System.out.println(assignments.getAccountingAssistant());
	}
	
	@Test
	public void assignments_readWhereFilterForUI(){
		Collection<Assignments> collection = 
				AssignmentsQuerier.getInstance().readWhereFilterForUI(new QueryExecutorArguments().setNumberOfTuples(1));
		Assignments assignments = collection.iterator().next();
		System.out.println(assignments.getIdentifier());
		System.out.println(assignments.getSectionAsString());
		System.out.println(assignments.getBudgetSpecializationUnitAsString());
		System.out.println(assignments.getActionAsString());
		System.out.println(assignments.getActivityAsString());
		System.out.println(assignments.getActivityCategoryAsString());
		System.out.println(assignments.getExpenditureNatureAsString());
		System.out.println(assignments.getEconomicNatureAsString());
		System.out.println(assignments.getAdministrativeUnitAsString());
		System.out.println(assignments.getCreditManagerHolderAsString());
		System.out.println(assignments.getCreditManagerAssistantAsString());
		System.out.println(assignments.getAuthorizingOfficerHolderAsString());
		System.out.println(assignments.getAuthorizingOfficerAssistantAsString());
		System.out.println(assignments.getFinancialControllerHolderAsString());
		System.out.println(assignments.getFinancialControllerAssistantAsString());
		System.out.println(assignments.getAccountingHolderAsString());
		System.out.println(assignments.getAccountingAssistantAsString());
	}
	
	@Test
	public void assignments_readWhereFilterForApplyModel(){
		Filter filter = new Filter();
		filter.addField(AssignmentsQuerier.PARAMETER_NAME_ECONOMIC_NATURE, "0");
		//filter.addField(AssignmentsQuerier.PARAMETER_NAME_CREDIT_MANAGER_HOLDER, "26010568");
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setFilter(filter);
		queryExecutorArguments.setQueryFromIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER);		
		LogHelper.logInfo(String.format("Compte des affectations à traiter en cours..."), getClass());
		Long t = System.currentTimeMillis();
		Long numberOfExecutionImputations = AssignmentsQuerier.getInstance().countWhereFilter(queryExecutorArguments);
		LogHelper.logInfo(String.format("%s affectations à traiter compté en %s", numberOfExecutionImputations,TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		if(NumberHelper.isLessThanOrEqualZero(numberOfExecutionImputations))
			return;
		Integer numberOfBatches = (int) (numberOfExecutionImputations / 3000) + (numberOfExecutionImputations % 3000 == 0 ? 0 : 1);
		LogHelper.logInfo(String.format("taille du lot est de %s. %s lot(s) à traiter",3000,numberOfBatches), getClass());
		queryExecutorArguments.setQueryFromIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER).setNumberOfTuples(3000);
		for(Integer index = 0; index < numberOfBatches; index = index + 1) {
			queryExecutorArguments.setFirstTupleIndex(index * 3000); 
			t = System.currentTimeMillis();
			Collection<Assignments> collection = AssignmentsQuerier.getInstance().readWhereFilterForApplyModel(queryExecutorArguments);
			LogHelper.logInfo(String.format("\tChargement de %s assignation(s) à partir l'index %s en %s",CollectionHelper.getSize(collection)
					,queryExecutorArguments.getFirstTupleIndex(),TimeHelper.formatDuration(System.currentTimeMillis() - t)), getClass());
		}
		/*Collection<Assignments> collection = 
				AssignmentsQuerier.getInstance().readWhereFilterForApplyModel(new QueryExecutorArguments().setNumberOfTuples(1));
		Assignments assignments = collection.iterator().next();
		System.out.println(assignments.getExecutionImputation().getIdentifier());
		System.out.println(assignments.getExecutionImputation().getCode());
		*/
	}
	
	@Test
	public void executionImputation_readIdentifiersNotInAssignments(){
		System.out.println(ExecutionImputationQuerier.getInstance().countIdentifiersNotInAssignments());
		Collection<ExecutionImputation> executionImputations = 
				ExecutionImputationQuerier.getInstance().readNotInAssignmentsForInitialization(new QueryExecutorArguments().setNumberOfTuples(1));
		System.out.println("SECTION : "+executionImputations.iterator().next().getSectionCodeName());	
	}
	
	@Test
	public void executionImputation_readByIdentifierForEdit(){
		ExecutionImputation executionImputation = ExecutionImputationQuerier.getInstance().readByIdentifierForEdit("IMPUTATION13002010027EEFC5BC4534B49FF9C9FCDC80878182E");
		System.out.println(executionImputation.getCreditManager().getHolder() != null);
		System.out.println(executionImputation.getCreditManager().getAssistant() != null);
		System.out.println(executionImputation.getAuthorizingOfficer().getHolder() != null);
		System.out.println(executionImputation.getAuthorizingOfficer().getAssistant() != null);
		System.out.println(executionImputation.getFinancialController().getHolder() != null);
		System.out.println(executionImputation.getFinancialController().getAssistant() != null);
		System.out.println(executionImputation.getAccounting().getHolder() != null);
		System.out.println(executionImputation.getAccounting().getAssistant() != null);
	}
	
	@Test
	public void function_readWhereAssociatedToScopeTypeForUICreateScopeFunction(){
		EntityReader.getInstance().readMany(Function.class, new QueryExecutorArguments().setQuery(new Query().setIdentifier(FunctionQuerier.QUERY_IDENTIFIER_READ_WHERE_ASSOCIATED_TO_SCOPE_TYPE_FOR_UI_CREATE_SCOPE_FUNCTION)));
	}
		
	@Test
	public void readProjection02WithBudgetaryFunctionsAndFunctionsByIdentifier(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		System.out.println(AccountRequestQuerier.getInstance().readProjection01WithBudgetaryFunctionsAndFunctionsByIdentifier("D_yy@y.com"));
	}
	
	@Test
	public void scopeFunction_readWhereFilter(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setQueryFromIdentifier(ScopeFunctionQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER);
		queryExecutorArguments.addFilterFieldsValues(ScopeFunctionQuerier.PARAMETER_NAME_FUNCTION_CODE,Function.CODE_AUTHORIZING_OFFICER_HOLDER);
		//queryExecutorArguments.addFilterFieldsValues(ScopeFunctionQuerier.PARAMETER_NAME_SCOPE_NAME,"trait dir inf");
		queryExecutorArguments.addFilterFieldsValues(ScopeFunctionQuerier.PARAMETER_NAME_SCOPE_CODE_NAME,"toumodi DUR");
		//queryExecutorArguments.addFilterFieldsValues(ScopeFunctionQuerier.PARAMETER_NAME_SCOPE_NAME,"abidjan");
		ScopeFunctionQuerier.getInstance().readWhereFilterForUI(queryExecutorArguments).forEach(x -> {System.out.println(x.getScopeAsString()+" - "+x.getFunctionAsString());});
	}
	
	@Test
	public void scopeFunction_readWhereFilterForUI_cf(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setQueryFromIdentifier(ScopeFunctionQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI);
		queryExecutorArguments.addFilterFieldsValues(ScopeFunctionQuerier.PARAMETER_NAME_FUNCTION_IDENTIFIER,"CF");
		ScopeFunctionQuerier.getInstance().readWhereFilterForUI(queryExecutorArguments).forEach(x -> {System.out.println(x+" - "+x);});
	}
	
	@Test
	public void scopeFunction_readByFunctionCodes(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		ScopeFunctionQuerier.getInstance().readByFunctionsCodes(Function.CODE_FINANCIAL_CONTROLLER_HOLDER).forEach(x -> {System.out.println(x);});
	}
	
	@Test
	public void scopeTypeFunction_read(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		System.out.println(EntityReader.getInstance().readMany(ScopeTypeFunction.class,ScopeTypeFunctionQuerier.QUERY_IDENTIFIER_READ));
		System.out.println(EntityReader.getInstance().readMany(ScopeTypeFunction.class,ScopeTypeFunctionQuerier.QUERY_IDENTIFIER_READ_FOR_UI));
		System.out.println(EntityCounter.getInstance().count(ScopeTypeFunction.class,ScopeTypeFunctionQuerier.QUERY_IDENTIFIER_COUNT));
		System.out.println(ScopeTypeFunctionQuerier.getInstance().readWhereScopeFunctionDerivableIsTrue());
	}
	
	@Test
	public void function_readByBusinessIdentifiers(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		FunctionQuerier.getInstance().readByBusinessIdentifiers(Function.class,List.of(Function.CODE_CREDIT_MANAGER_HOLDER
				,Function.CODE_AUTHORIZING_OFFICER_HOLDER,Function.CODE_FINANCIAL_CONTROLLER_HOLDER,Function.CODE_ACCOUNTING_HOLDER))
			.forEach(x -> {System.out.println(x);});
	}
	
	@Test
	public void executionImputation_readWhereFilter(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		org.cyk.utility.__kernel__.persistence.query.QueryExecutor.AbstractImpl.LOG_LEVEL = java.util.logging.Level.INFO;
		Long t = System.currentTimeMillis();
		System.out.println("STARTS");
		Collection<ExecutionImputation> executionImputations = EntityReader.getInstance().readMany(ExecutionImputation.class,new QueryExecutorArguments()
				.setQueryFromIdentifier(ExecutionImputationQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER)
				.setNumberOfTuples(20));
		executionImputations.forEach(x -> {
					System.out.println(x.getIdentifier()+" - "+ x.getCreditManagerHolderScopeFunctionCodeName()+" : "+x.getAuthorizingOfficerHolderScopeFunctionCodeName()
							+" : "+x.getFinancialControllerHolderScopeFunctionCodeName()+" : "+x.getAccountingHolderScopeFunctionCodeName());
				});
		System.out.println((System.currentTimeMillis() - t)/1000);
	}
	/*
	@Test
	public void executionImputation_readWhereFilterAll(){
		org.cyk.utility.__kernel__.persistence.query.QueryExecutor.AbstractImpl.LOG_LEVEL = java.util.logging.Level.INFO;
		Long t = System.currentTimeMillis();
		System.out.println("STARTS");
		Collection<ExecutionImputation> executionImputations = EntityReader.getInstance().readMany(ExecutionImputation.class,new QueryExecutorArguments().setQuery(new Query().setIdentifier(ExecutionImputationQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_WITH_ALL))
				.setNumberOfTuples(20));
		executionImputations.forEach(x -> {
					System.out.println(x.getIdentifier()+" - "+ x.getCreditManager().getHolder().getCode()+" : "+x.getAuthorizingOfficer().getHolder().getCode()
							+" : "+x.getFinancialController().getHolder().getCode()+" : "+x.getAccounting().getHolder().getCode());
				});
		System.out.println((System.currentTimeMillis() - t)/1000);
	}
	*/
	@Test
	public void executionImputation_readAll(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		ExecutionImputationQuerier.getInstance().read();
		//__inject__(ExecutionImputationPersistence.class).read();
	}
	
	@Test
	public void executionImputation_readForEdit(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		ExecutionImputation executionImputation = EntityReader.getInstance().readOne(ExecutionImputation.class,new QueryExecutorArguments()
				.setQueryFromIdentifier(ExecutionImputationQuerier.QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT)
				.addFilterFieldsValues(ExecutionImputationQuerier.PARAMETER_NAME_IDENTIFIER,"IMPUTATION11018010005BF1E3A5D576B48E1B5DC3D4FDA69A2CF"));
		System.out.println(executionImputation.getName());
		System.out.println(executionImputation.getFunctions());
	}
	
	@Test
	public void showFunctionWithAll(){
		System.out.println("--------------------- Function with all ---------------------");
		Collection<Function> functions = EntityReader.getInstance().readMany(Function.class, new QueryExecutorArguments().setQuery(new Query()
				.setIdentifier(FunctionQuerier.QUERY_IDENTIFIER_READ_WHERE_ASSOCIATED_TO_SCOPE_TYPE_WITH_ALL)));
		if(CollectionHelper.isNotEmpty(functions))
			functions.forEach(function -> {
				System.out.println(function+" : "+function.getScopeTypes());
			});
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		Collection<Scope> scopes = ScopeQuerier.getInstance().readWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifier(
				new QueryExecutorArguments().setQueryFromIdentifier(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_AND_NOT_ASSOCIATED_TO_FUNCTION_BY_TYPE_IDENTIFIER)
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_FUNCTION_IDENTIFIER,"CF",ScopeQuerier.PARAMETER_NAME_TYPE_IDENTIFIER,"SECTION"));
		if(CollectionHelper.isNotEmpty(scopes)) {
			System.out.println(scopes.size());
			//scopes.forEach(scope -> {
			//	System.out.println(scope);
			//});
		}
	}
	
	@Test
	public void counts(){
		System.out.println("--------------------- Counts ---------------------");
		System.out.println("Actors : "+EntityCounter.getInstance().count(Actor.class, ActorQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER));
		System.out.println("AccountRequests : "+EntityCounter.getInstance().count(AccountRequest.class, AccountRequestQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER));
		System.out.println("RejectedAccountRequests : "+EntityCounter.getInstance().count(RejectedAccountRequest.class, RejectedAccountRequestQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER));
	}
	
	@Test
	public void scopeQuerier_USBs(){
		System.out.println("--------------------- USBs ---------------------");
		Collection<Scope> scopes = ScopeQuerier.getInstance().readWhereTypeIsUSBAndFilter(new QueryExecutorArguments()
				.setQueryFromIdentifier(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_USB_AND_FILTER));
		if(scopes != null)
			scopes.forEach(scope -> {
				System.out.println(scope.getCode()+" : "+scope.getSectionAsString());
			});
	}
	
	@Test
	public void scopeQuerier_ACTIVITYs(){
		System.out.println("--------------------- ACTIVITYs ---------------------");
		Collection<Scope> scopes = ScopeOfTypeActivityQuerier.getInstance().readWhereFilter(new QueryExecutorArguments());
		if(scopes != null)
			System.out.println("COUNT : "+scopes.size());
			/*scopes.forEach(scope -> {
				System.out.println(scope.getCode()+" : "+scope.getSectionAsString());
			});*/
	}
	
	@Test
	public void scopeQuerier_readVisibleSectionsWhereFilter(){
		assertVisibleSectionsWhereFilter("kycdev@gmail.com","327","103");
		assertVisibleSectionsWhereFilter("kb@m.com","108");
	}
	
	@Test
	public void scopeQuerier_readInvisibleSectionsWhereFilter(){
		//assertInvisibleSectionsWhereFilter("kycdev@gmail.com",null,new String[] {"101","102"});
		//assertInvisibleSectionsWhereFilter("kb@m.com",null,new String[] {"102","103","103"});
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		assertInvisibleSectionsWhereFilter("kb@m.com","102",new String[] {});
	}
	
	@Test
	public void scopeQuerier_readVisibleAdministrativeUnitsWhereFilter(){
		assertVisibleAdministrativeUnitsWhereFilter("kycdev@gmail.com","11010016");
	}
	
	@Test
	public void scopeQuerier_readInvisibleAdministrativeUnitsWhereFilter(){
		assertInvisibleAdministrativeUnitsWhereFilter("kycdev@gmail.com","11010001");
	}
	
	@Test
	public void scopeQuerier_readVisibleBudgetSpecializationUnitsWhereFilter(){
		assertVisibleBudgetSpecializationUnitsWhereFilter("kycdev@gmail.com","14970","14984","15022","21083","22084","13004");
	}
	
	@Test
	public void scopeQuerier_readInvisibleBudgetSpecializationUnitsWhereFilter(){
		assertInvisibleBudgetSpecializationUnitsWhereFilter("kycdev@gmail.com","01001");
	}
}