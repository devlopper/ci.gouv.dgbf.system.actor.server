package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ClusterPrivilegeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ClusterPrivilegeDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ClusterPrivilegeRepresentationImpl extends AbstractRepresentationEntityImpl<ClusterPrivilegeDto> implements ClusterPrivilegeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
