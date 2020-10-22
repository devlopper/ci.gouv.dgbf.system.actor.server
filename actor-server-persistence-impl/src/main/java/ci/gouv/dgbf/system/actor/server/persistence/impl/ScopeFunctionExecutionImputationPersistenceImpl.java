package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.persistence.EntityManager;

import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeFunctionExecutionImputationPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionExecutionImputation;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityCreator;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;
import org.cyk.utility.server.persistence.PersistenceServiceProvider;

@ApplicationScoped
public class ScopeFunctionExecutionImputationPersistenceImpl extends AbstractPersistenceEntityImpl<ScopeFunctionExecutionImputation> implements ScopeFunctionExecutionImputationPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	@Inject
	private EntityManager entityManager;

	@Override
	public PersistenceServiceProvider<ScopeFunctionExecutionImputation> createMany(Collection<ScopeFunctionExecutionImputation> scopeFunctionExecutionImputations, Properties properties) {
		EntityCreator.getInstance().createMany(new QueryExecutorArguments().setObjects(CollectionHelper.cast(Object.class, scopeFunctionExecutionImputations))
				.setEntityManager(entityManager));
		return this;
	}
	
}