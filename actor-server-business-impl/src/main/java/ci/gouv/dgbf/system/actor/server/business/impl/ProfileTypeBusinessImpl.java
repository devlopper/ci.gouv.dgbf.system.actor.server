package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.business.server.AbstractSpecificBusinessImpl;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.QueryExecutorArguments;

import ci.gouv.dgbf.system.actor.server.business.api.ProfileTypeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;

@ApplicationScoped
public class ProfileTypeBusinessImpl extends AbstractSpecificBusinessImpl<ProfileType> implements ProfileTypeBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Collection<ProfileType> get(Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples) {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(ProfileType.class).page(pageable, firstTupleIndex, numberOfTuples);
		return EntityReader.getInstance().readMany(ProfileType.class, arguments);
	}
}