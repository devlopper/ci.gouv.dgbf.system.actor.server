package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public class DataUnitTest extends AbstractUnitTest {
	private static final long serialVersionUID = 1L;

	@Test
	public void dataCountIsGreaterThanZero(){
		assertThatCountIsGreaterThanZero(Section.class,Scope.class,Function.class,ScopeFunction.class,ExecutionImputation.class,Assignments.class);
	}
}