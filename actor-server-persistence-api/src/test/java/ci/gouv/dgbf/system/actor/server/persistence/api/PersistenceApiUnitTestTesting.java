package ci.gouv.dgbf.system.actor.server.persistence.api;

import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.SectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public class PersistenceApiUnitTestTesting extends AbstractPersistenceApiUnitTestValidate {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "test";
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