package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.util.Collection;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.SectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public class PersistenceApiUnitTestTesting extends AbstractPersistenceApiUnitTestValidate {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "test";
	}
	
	@Test
	public void assignments_readWhereFilter(){
		AssignmentsQuerier.getInstance().readWhereFilter(new QueryExecutorArguments().setNumberOfTuples(1));
	}
	
	@Test
	public void assignments_readWhereFilter_ACTIVITE22082030014(){
		Collection<Assignments> collection = 
				AssignmentsQuerier.getInstance().readWhereFilter(new QueryExecutorArguments().addFilterField(AssignmentsQuerier.PARAMETER_NAME_ACTIVITY, "ACTIVITE22082030014"));
		System.out.println("Size : "+CollectionHelper.getSize(collection));
		if(collection != null)
			collection.forEach(x -> {
				System.out.println(x.getExecutionImputation().getActivityCode()+" - "+x.getExecutionImputation().getEconomicNatureCode());
			});
	}
	
	@Test
	public void request_readByIdentifierForUI(){
		Request request = RequestQuerier.getInstance().readByIdentifierForUI("f451673d-7330-4ac6-adec-55ac25422ce9");
		System.out.println(request.getActorCode());
		System.out.println(request.getActorNames());
		System.out.println(request.getStatusAsString());
		System.out.println(request.getTypeAsString());
		System.out.println(request.getComment());
		System.out.println(request.getCreationDateAsString());
		System.out.println(request.getBudgetariesScopeFunctionsAsStrings());
		System.out.println(request.getBudgetariesScopeFunctionsGrantedAsStrings());
		/*
		System.out.println(request.getHasBudgetaryScopeFunctionWhereFunctionCodeIsCreditManagerHolder());
		System.out.println(request.getHasBudgetaryScopeFunctionWhereFunctionCodeIsAuthorizingOfficerHolder());
		System.out.println(request.getHasBudgetaryScopeFunctionWhereFunctionCodeIsFinancialControllerHolder());
		System.out.println(request.getHasBudgetaryScopeFunctionWhereFunctionCodeIsAccountingHolder());
		*/
	}
	
	@Test
	public void request_countWhereFilterForUI_sections(){
		for(Section section : SectionQuerier.getInstance().read()) {
			System.out.println(section.getCode());	
			for(String functionIdentifier : new String[] {"GC","OD"}) {
				Long count = RequestQuerier.getInstance().countWhereFilter(new QueryExecutorArguments()
						.setQueryFromIdentifier(RequestQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER)
						.addFilterFieldsValues(RequestQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT_SECTION_IDENTIFIER,section.getIdentifier()
								,RequestQuerier.PARAMETER_NAME_FUNCTION_IDENTIFIER,functionIdentifier));
				System.out.println("\t"+functionIdentifier+" : "+count);	
			}
			
		}
		
	}
}