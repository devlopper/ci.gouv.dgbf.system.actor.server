package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.stream.Collectors;

import javax.persistence.PersistenceException;

import org.cyk.utility.__kernel__.computation.SortOrder;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.MetricsManager;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.LocalityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ScopeAdministrativeUnitSectionsReader;

public class PersistenceImplUnitTestDev extends AbstractUnitTestLive {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Test
	public void request_sort_firstNameAndLastNames(){
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_FIRST_NAME_AND_LAST_NAMES)
		.addProcessableTransientFieldsNames(Request.FIELD_FIRST_NAME_AND_LAST_NAMES);
		arguments.setSortOrders(Map.of(Request.FIELD_FIRST_NAME_AND_LAST_NAMES,SortOrder.ASCENDING));
		arguments.setNumberOfTuples(10);
		Collection<Request> requests = EntityReader.getInstance().readMany(Request.class, arguments);
		requests.forEach(r -> {
			System.out.println(r.getFirstNameAndLastNames());
		});
	}
	
	@Test
	public void request_assertHolders(){
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadOneDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER);	
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_AS_CODE_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES_IS_CREDIT_MANAGER_HOLDER_IS_AUTHORIZING_OFFICER_HOLDER_IS_FINANCIAL_CONTROLLER_HOLDER_IS_ACCOUNTING_HOLDER
				,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES,Request.FIELD_DISPATCH_SLIP_CODE);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_IDENTIFIER, "ff7eb43d-1521-4566-b28e-50f6f4fa77e8");
		Request request = EntityReader.getInstance().readOne(Request.class, arguments);
		assertThat(request.getIsCreditManagerHolder()).isTrue();
		assertThat(request.getIsAuthorizingOfficerHolder()).isNull();
		assertThat(request.getIsFinancialControllerHolder()).isNull();
		assertThat(request.getIsAccountingHolder()).isNull();
	}
	
	@Test
	public void scopeFunctionQuerier_readMaxOrderNumberByFunctionCode_GC(){
		assert_scopeFunctionQuerier_readMaxOrderNumberByFunctionCode("GC", 8526);
	}
	
	@Test
	public void scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith_G1(){
		assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith("G1", "G108526");
	}
	
	@Test
	public void scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith_G2(){
		assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith("G2", "G200751");
	}
	
	@Test
	public void scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith_G3(){
		assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith("G3", "G301190");
	}
	
	@Test
	public void scopeFunctionQuerier_readMaxOrderNumberByFunctionCode_OD(){
		assert_scopeFunctionQuerier_readMaxOrderNumberByFunctionCode("ORD", 2515);
	}
	
	@Test
	public void scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith_O2(){
		assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith("O2", "O202515");
	}
	
	@Test
	public void scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith_O3(){
		assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith("O3", "O302514");
	}
	
	@Test
	public void scopeFunctionQuerier_readByIdentifierForUI(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setQueryFromIdentifier(ScopeFunctionQuerier.QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI);
		queryExecutorArguments.addFilterFieldsValues(ScopeFunctionQuerier.PARAMETER_NAME_IDENTIFIER,"G100761");
		queryExecutorArguments.addProcessableTransientFieldsNames(ScopeFunction.FIELD_ACTORS_CODES);
		ScopeFunction scopeFunction = ScopeFunctionQuerier.getInstance().readOne(queryExecutorArguments);
		assertThat(scopeFunction.getIdentifier()).isNotBlank();
		assertThat(scopeFunction.getActorsCodes()).containsExactly("yay.diomande@budget.gouv.ci");
	}
	
	@Test
	public void localities_regions(){
		Collection<Locality> regions = EntityReader.getInstance().readManyDynamically(Locality.class, new QueryExecutorArguments()
				.addFilterFieldsValues(LocalityQuerier.PARAMETER_NAME_TYPE,Locality.Type.REGION));
		assertThat(regions).isNotNull();
	}

	@Test
	public void localities_regions_typeAsString(){
		Collection<Locality> regions = EntityReader.getInstance().readManyDynamically(Locality.class, new QueryExecutorArguments()
				.addFilterFieldsValues(LocalityQuerier.PARAMETER_NAME_TYPE,Locality.Type.REGION.name()));
		assertThat(regions).isNotNull();
	}
	
	@Test
	public void administrativeUnitQuerier_readByIdentifierWithCodesNamesForUI(){
		AdministrativeUnit administrativeUnit = AdministrativeUnitQuerier.getInstance().readByIdentifierWithCodesNamesForUI("UAba28d795-8bec-4550-afa1-b76e8f8e8de6");
		assertThat(administrativeUnit).isNotNull();
		assertThat(administrativeUnit.getSection()).isNotNull();
		assertThat(administrativeUnit.getSubPrefecture()).isNotNull();
		assertThat(administrativeUnit.getDepartment()).isNotNull();
		assertThat(administrativeUnit.getRegion()).isNotNull();
		
		assertThat(administrativeUnit.getCode()).isEqualTo("26080841");
		assertThat(administrativeUnit.getName()).isEqualTo("Ecoles Primaires Publiques (EPP) Tanda");
		
		assertThat(administrativeUnit.getSection().getCode()).isEqualTo("331");
		assertThat(administrativeUnit.getSection().getName()).isEqualTo("Ministère de l'Education Nationale et de l'Alphabétisation");
		
		assertThat(administrativeUnit.getSubPrefecture().getCode()).isEqualTo("620501");
		assertThat(administrativeUnit.getSubPrefecture().getName()).isEqualTo("Sous-préfecture de Tanda ");
		
		assertThat(administrativeUnit.getDepartment().getCode()).isEqualTo("6205");
		assertThat(administrativeUnit.getDepartment().getName()).isEqualTo("DEPARTEMENT DE TANDA");
		
		assertThat(administrativeUnit.getRegion().getCode()).isEqualTo("62");
		assertThat(administrativeUnit.getRegion().getName()).isEqualTo("REGION DU GONTOUGO");
	}
	
	@Test
	public void assignments_export_entityManagerNotProvided() {
		MetricsManager.getInstance().enable();
		for(Integer index = 0; index < 100; index = index + 1) {
			AssignmentsQuerier.getInstance().export("test", "test", "test", new Date(), null);
		}
		MetricsManager.getInstance().disable();
	}
	
	@Test
	public void assignments_export_entityManagerProvided() {
		Assertions.assertThrows(PersistenceException.class, () -> {
			for(Integer index = 0; index < 100; index = index + 1) {
				AssignmentsQuerier.getInstance().export("test", "test", "test", new Date(), EntityManagerGetter.getInstance().get());
			}
		});
	}
	
	@Test
	public void scopeAdministrativeUnitSectionsReader() {
		Collection<Scope> scopes = EntityReader.getInstance().readMany(AdministrativeUnit.class).stream().map(a -> new Scope()
				.setIdentifier(a.getIdentifier()).setCode(a.getCode())).collect(Collectors.toList());
		//__inject__(ScopeQuerier.class).readByTypesCodes(List.of("UA"));
		new ScopeAdministrativeUnitSectionsReader().readThenSet(scopes, null);
		scopes.forEach(s -> {
			System.out.println("PersistenceImplUnitTestDev.scopeAdministrativeUnitSectionsReader() : "+s.getCode()+" - "+s.getSectionAsString());
		});
	}
}