package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import org.cyk.utility.persistence.server.query.string.JoinStringBuilder;
import org.cyk.utility.test.weld.AbstractWeldUnitTest;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;

public class QueryStringUnitTest extends AbstractWeldUnitTest {

	@Test
	public void queriesStrings(){
		assertThat(JoinStringBuilder.getInstance().build(new JoinStringBuilder.Arguments()
				.setMasterVariableName("t").setMasterFieldName(Assignments.FIELD_CREDIT_MANAGER_HOLDER).setTupleName("ScopeFunction")))
		.isEqualTo("LEFT JOIN ScopeFunction creditManagerHolder ON creditManagerHolder = t.creditManagerHolder");
	}
	
}
