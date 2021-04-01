package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import java.sql.SQLIntegrityConstraintViolationException;
import java.util.Collection;
import java.util.List;

import org.cyk.utility.persistence.query.EntityCreator;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class PersistenceUnitTestDev extends AbstractUnitTestValidate {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Test
	public void scopeFunctionQuerier_readActorsCodes(){
		Collection<ScopeFunction> scopeFunctions = EntityReader.getInstance().readMany(ScopeFunction.class, new QueryExecutorArguments()
				.setProcessableTransientFieldsNames(List.of(ScopeFunction.FIELD_ACTORS_CODES)));
		scopeFunctions.forEach(x -> {
			System.out.println(x.getActorsCodes());
		});
	}
	
	//@Test
	public void doublon() {
		try {
			EntityCreator.getInstance().createOneInTransaction(new ProfileType().setCode(ProfileType.CODE_SYSTEME));
		} catch (Exception e) {
			Throwable index = e;
			while(index != null) {
				System.out.println(index.getClass());
				if(index instanceof java.sql.SQLIntegrityConstraintViolationException) {
					java.sql.SQLIntegrityConstraintViolationException i = (SQLIntegrityConstraintViolationException) index;
					System.out.println("SQLICVE : "+i.getErrorCode()+" : "+i.getMessage());
				}
				index = index.getCause();
			}
			e.printStackTrace();
		}
	}
}