package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.business.server.AbstractSpecificBusinessImpl;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.QueryExecutorArguments;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeTypeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

@ApplicationScoped
public class ScopeTypeBusinessImpl extends AbstractSpecificBusinessImpl<ScopeType> implements ScopeTypeBusiness,Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public Collection<ScopeType> get(Boolean requestable,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples) {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(ScopeType.class)
				.filterIfNotNull(ScopeTypeQuerier.PARAMETER_NAME_REQUESTABLE,requestable)
				.page(pageable, firstTupleIndex, numberOfTuples);
		return EntityReader.getInstance().readMany(getEntityClass(), arguments);
	}
}