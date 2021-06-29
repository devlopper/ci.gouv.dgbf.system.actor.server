package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.object.marker.AuditableWhoDoneWhatWhen;
import org.cyk.utility.persistence.query.EntityCreator;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.EntityUpdater;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.test.persistence.server.Transaction;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExpenditureNatureQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.SectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExpenditureNature;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public class ScopeFunctionPersistenceImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@Test
	public void sectionQuerier_read(){
		assertThat(SectionQuerier.getInstance().read().stream().map(Section::getCode)
				.collect(Collectors.toList())).contains("101");
	}
	
	@Test
	public void functionQuerier_readExecutionHolders(){
		assertThat(FunctionQuerier.getInstance().readExecutionHolders(null).stream().map(Function::getCode)
				.collect(Collectors.toList())).containsExactly("GC","ORD","CF","CPT");
	}
	
	@Test
	public void functionQuerier_readExecutionHoldersAndAssistants(){
		assertThat(FunctionQuerier.getInstance().readExecutionHoldersAndAssistants(null).stream().map(Function::getCode)
				.collect(Collectors.toList())).containsExactly("GC","AGC","ORD","AORD","CF","ACF","CPT","ACPT");
	}
	
	@Test
	public void administrativeUnitQuerier_read(){
		assertThat(EntityReader.getInstance().readMany(AdministrativeUnit.class).stream().map(AdministrativeUnit::getCode)
				.collect(Collectors.toList())).contains("DTI");
	}
	
	@Test
	public void expenditureNatureQuerier_readAllForUI(){
		assertThat(ExpenditureNatureQuerier.getInstance().readAllForUI().stream().map(ExpenditureNature::getCode)
				.collect(Collectors.toList())).contains("1","2","3","4");
	}
	
	@Test
	public void scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith(){
		assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith("G1", "G100762");
		assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith("G2", "G200751");
		assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith("G3", "G301190");
	}
	
	@Test
	public void scopeFunctionPersistence_readByScopeIdentifierByFunctionIdentifier(){
		Collection<ScopeFunction> collection = ScopeFunctionQuerier.getInstance().readByScopeIdentifierByFunctionIdentifier(new QueryExecutorArguments()
				.addFilterFieldsValues(ScopeFunctionQuerier.PARAMETER_NAME_SCOPE_IDENTIFIER,"UA68a6d9e7-420a-4bd9-9c01-12cfdad33fb9"
						,ScopeFunctionQuerier.PARAMETER_NAME_FUNCTION_IDENTIFIER,"GC"));
		assertThat(collection).hasSize(1);
		assertThat(collection.iterator().next().getCode()).isEqualTo("G100762");
	}
	
	@Test
	public void scopeFunctionPersistence_computeCreditManagerHolderName(){
		assertThat(__inject__(ScopeFunctionPersistence.class).computeCreditManagerHolderName("DTI"))
			.isEqualTo("Gestionnaire de crédits Direction des traitements informatiques");
	}
	
	@Test
	public void scopeFunctionPersistence_computeAuthorizingOfficerHolderName_delegue_administrationGenerale(){
		assertThat(__inject__(ScopeFunctionPersistence.class).computeAuthorizingOfficerHolderName("USB7d152f5a-3bcb-4ba3-a107-b680b6a230b3",null))
		.isEqualTo("Ordonnateur délégué du Programme Administration générale du Sénat");
	}
	
	@Test
	public void scopeFunctionPersistence_computeAuthorizingOfficerHolderName_delegue(){
		assertThat(__inject__(ScopeFunctionPersistence.class).computeAuthorizingOfficerHolderName("USB7d152f5a-3bcb-4ba3-a107-b680b6a230b2",null))
		.isEqualTo("Ordonnateur délégué du Programme Budget");
	}
	
	@Test
	public void scopeFunctionPersistence_computeAuthorizingOfficerHolderName_secondaire(){
		assertThat(__inject__(ScopeFunctionPersistence.class).computeAuthorizingOfficerHolderName("USB7d152f5a-3bcb-4ba3-a107-b680b6a230b2", "DIMBOKRO"))
			.isEqualTo("Ordonnateur secondaire du Programme Budget a Dimbokro");
	}
	
	@Test
	public void scopeFunctionQuerier_readActorsCodes(){
		ScopeFunction scopeFunction = EntityReader.getInstance().readOne(ScopeFunction.class, new QueryExecutorArguments()
				.setQueryFromIdentifier(ScopeFunctionQuerier.QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI)
				.addFilterFieldsValues(ScopeFunctionQuerier.PARAMETER_NAME_IDENTIFIER,"A3002110")
				.setProcessableTransientFieldsNames(List.of(ScopeFunction.FIELD_ACTORS_CODES)));
		assertThat(scopeFunction).isNotNull();
		assertThat(scopeFunction.getActorsCodes()).containsExactly("kycdev@gmail.com");
	}
	
	@Test
	public void scopeFunctionQuerier_readByBudgetSpecializationUnitIdentifier() {
		QueryExecutorArguments arguments = new QueryExecutorArguments()
				.setQueryFromIdentifier(ScopeFunctionQuerier.QUERY_IDENTIFIER_READ_BY_FUNCTION_IDENTIFIER_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER);
		arguments.addFilterFieldsValues(ScopeFunctionQuerier.PARAMETER_NAME_FUNCTION_IDENTIFIER,"ORD"
				,ScopeFunctionQuerier.PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER,"USB7d152f5a-3bcb-4ba3-a107-b680b6a230b2"
				);
		Collection<ScopeFunction> scopeFunctions = EntityReader.getInstance().readMany(ScopeFunction.class, arguments);
		assertThat(scopeFunctions).isNotEmpty();
		assertThat(FieldHelper.readBusinessIdentifiersAsStrings(scopeFunctions)).containsExactly("O3001","O402500","O502501");
	}
	
	@Test
	public void updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers_trueBecomesFalse() {
		assertRequestScopeFunctionGranted("1", Boolean.TRUE);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				RequestScopeFunctionQuerier.getInstance().updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(entityManager,"user","maj","update",null,"O3001");
			}
		}.run();
		assertRequestScopeFunctionGranted("1", Boolean.FALSE);
		assertAudits(RequestScopeFunction.class, "1", "user", "maj","update");
	}
	
	@Test
	public void updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers_nullStaysNull() {
		assertRequestScopeFunctionGranted("2", null);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				RequestScopeFunctionQuerier.getInstance().updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(entityManager,"user","maj","update",null,"C100006");
			}
		}.run();
		assertRequestScopeFunctionGranted("2", null);
	}
	
	@Test
	public void updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers_falseStatysFalse() {
		assertRequestScopeFunctionGranted("3", Boolean.FALSE);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				RequestScopeFunctionQuerier.getInstance().updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(entityManager,"user","maj","update",null,"C100001");
			}
		}.run();
		assertRequestScopeFunctionGranted("3", Boolean.FALSE);
	}
	
	@Test
	public void audit_byInstance() {
		String identifier = "sfId01_abi";
		ScopeFunction scopeFunction = null;
		scopeFunction = EntityReader.getInstance().readOneDynamically(ScopeFunction.class, new QueryExecutorArguments()
				.addFilterFieldsValues(Querier.PARAMETER_NAME_IDENTIFIER,identifier).addProcessableTransientFieldsNames(AuditableWhoDoneWhatWhen.FIELD___AUDIT_RECORDS__));
		assertThat(scopeFunction).isNull();
		
		scopeFunction = new ScopeFunction();		
		scopeFunction.setIdentifier(identifier);
		scopeFunction.setCode("sf01");
		scopeFunction.setName("mysf 01");
		scopeFunction.setScope(EntityFinder.getInstance().find(Scope.class, "O3001"));
		scopeFunction.setFunction(EntityFinder.getInstance().find(Function.class, "ORD"));
		scopeFunction.setDocumentNumber(1);
		EntityCreator.getInstance().createOneInTransaction(scopeFunction);
		
		scopeFunction = EntityReader.getInstance().readOneDynamically(ScopeFunction.class, new QueryExecutorArguments()
				.addFilterFieldsValues(Querier.PARAMETER_NAME_IDENTIFIER,identifier).addProcessableTransientFieldsNames(AuditableWhoDoneWhatWhen.FIELD___AUDIT_RECORDS__));
		assertThat(scopeFunction).isNotNull();
		assertThat(scopeFunction.get__auditRecords__()).hasSize(1);
		assertThat(CollectionHelper.getElementAt(scopeFunction.get__auditRecords__(), 0).getCode()).isEqualTo("sf01");
		assertThat(CollectionHelper.getElementAt(scopeFunction.get__auditRecords__(), 0).getName()).isEqualTo("mysf 01");
		
		scopeFunction.setName("New Name");
		EntityUpdater.getInstance().updateOneInTransaction(scopeFunction);
		
		scopeFunction = EntityReader.getInstance().readOneDynamically(ScopeFunction.class, new QueryExecutorArguments()
				.addFilterFieldsValues(Querier.PARAMETER_NAME_IDENTIFIER,identifier).addProcessableTransientFieldsNames(AuditableWhoDoneWhatWhen.FIELD___AUDIT_RECORDS__));
		assertThat(scopeFunction).isNotNull();
		assertThat(scopeFunction.get__auditRecords__()).hasSize(2);
		assertThat(CollectionHelper.getElementAt(scopeFunction.get__auditRecords__(), 0).getCode()).isEqualTo("sf01");
		assertThat(CollectionHelper.getElementAt(scopeFunction.get__auditRecords__(), 0).getName()).isEqualTo("New Name");
		assertThat(CollectionHelper.getElementAt(scopeFunction.get__auditRecords__(), 1).getCode()).isEqualTo("sf01");
		assertThat(CollectionHelper.getElementAt(scopeFunction.get__auditRecords__(), 1).getName()).isEqualTo("mysf 01");
	}
	
	@Test
	public void audit_byDates(){
		List<LocalDateTime> dateTimes = new ArrayList<>();
		String identifier = "sfId01";
		assertAudit(ScopeFunction.class,null,null,0);
		
		ScopeFunction data = new ScopeFunction();		
		data.setIdentifier(identifier);
		data.setCode("sf01");
		data.setName("mysf 01");
		data.setScope(EntityFinder.getInstance().find(Scope.class, "O3001"));
		data.setFunction(EntityFinder.getInstance().find(Function.class, "ORD"));
		data.setDocumentNumber(1);
		addPause(dateTimes);
		EntityCreator.getInstance().createOneInTransaction(data);
		data = EntityFinder.getInstance().find(ScopeFunction.class,identifier);
		
		assertThat(data).isNotNull();
		assertAudit(ScopeFunction.class,null,null,1);
		
		data.setCode("code01").set__auditWhen__(null);
		addPause(dateTimes);
		EntityUpdater.getInstance().updateOneInTransaction(data);
		assertAudit(ScopeFunction.class,null,null,2);
		
		data.setCode("code 01").set__auditWhen__(null);
		addPause(dateTimes);
		EntityUpdater.getInstance().updateOneInTransaction(data);
		
		addPause(dateTimes);
		
		assertAudit(ScopeFunction.class,null,null,3);
		addPause(dateTimes);
		
		assertAudit(ScopeFunction.class,dateTimes.get(0),dateTimes.get(4),3);		
		assertAudit(ScopeFunction.class,dateTimes.get(0),dateTimes.get(3),3);
		assertAudit(ScopeFunction.class,dateTimes.get(0),dateTimes.get(2),2);
		assertAudit(ScopeFunction.class,dateTimes.get(0),dateTimes.get(1),1);
		assertAudit(ScopeFunction.class,dateTimes.get(3),dateTimes.get(4),0);
	}
}