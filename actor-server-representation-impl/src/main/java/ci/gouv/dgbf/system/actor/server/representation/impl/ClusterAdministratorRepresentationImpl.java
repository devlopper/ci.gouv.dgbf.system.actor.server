package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ClusterAdministratorRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ClusterAdministratorDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ClusterAdministratorRepresentationImpl extends AbstractRepresentationEntityImpl<ClusterAdministratorDto> implements ClusterAdministratorRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
